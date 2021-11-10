using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Linq;

namespace Empire
{
    class Game
    {
        // Map is the master map.
        public Map map;

        // playerMap is the player's map. It shows only what the player has seen plus
        // their own units and cities
        public Map playerMap;

        // enemyMap is the computer's map. It shows only what the computer has seen plus
        // their own units and cities
        public Map enemyMap;

        public Dictionary<int, Unit> units;

        private City playerCity;
        public City PlayerCity { get { return playerCity; } }
        private int unitID;
        private readonly Presentation presentation;

        public Game(Presentation presentation)
        {
            map = new Map();
            units = new Dictionary<int, Unit>();
            GameDate = 0;
            unitID = 0;
            this.presentation = presentation;
        }

        public int GameDate { get; set; }

        private enum ActionType
        {
            AircraftTransfer,
            ArmyTransfer,
            AttackCity,
            AttackUnit,
            Dock,
            EmbarkHelicopter,
            EmbarkTransport,
            Fill,
            Invalid,
            LandAirbase,
            LandVessel,
            Move,
            None
        }

        //---------------------------------------------------------------------
        // AttackCity
        // ==========
        //
        // Resolves an attack on a city
        //---------------------------------------------------------------------
        private void AttackCity(Army attacker, City city)
        {
            Debug.WriteLine("Siege begins...");

            if (city.alignment == Alignment.Player)
            {
                presentation.DisplayMessage($"Your city at ({city.x},{city.y}) is under attack");
            }

            Random rand = new Random();
            bool victory = false;

            do
            {
                int attackerRoll = rand.Next(1, 100);
                int defenderRoll = rand.Next(1, 100);

                // Attacker wins this round
                if (attackerRoll > defenderRoll)
                {
                    city.hits -= attacker.damage;
                    Debug.WriteLine($"Attacker wins {attackerRoll} vs {defenderRoll}: City hits {city.hits} remaining");
                }

                // city wins this round
                else if (defenderRoll > attackerRoll)
                {
                    attacker.hits -= city.damage;
                    Debug.WriteLine($"City wins {defenderRoll} vs {attackerRoll}: Attacking army destroyed");
                }

                // city captured
                if (city.hits <= 0)
                {
                    victory = true;

                    if (city.alignment == Alignment.Player)
                    {
                        presentation.DisplayMessage($"Your city at ({city.x},{city.y}) was captured");
                    }

                    // Change ownership of the city and all its occupants
                    city.alignment = attacker.alignment;

                    foreach (Unit unit in city.occupants)
                    {
                        unit.alignment = attacker.alignment;
                    }

                    if (city.alignment == Alignment.Player)
                    {
                        ChooseCityProduction(city);
                    }
                    else if (city.alignment == Alignment.Enemy)
                    {
                        ChooseEnemyCityProduction(city);
                    }
                }

                // Attacker destroyed
                if (attacker.hits <= 0)
                {
                    attacker.Destroyed = true;
                    victory = true;

                    if (attacker.alignment == Alignment.Player)
                    {
                        presentation.DisplayMessage($"Your army was destroyed");
                    }
                    else if (city.alignment == Alignment.Player)
                    {
                        presentation.DisplayMessage($"Your city at ({city.x},{city.y}) repulsed the attack");
                    }
                }

            } while (!victory);
        }

        //---------------------------------------------------------------------
        // ChooseCities
        // ============
        //
        // Choose the starting cities for each player
        //---------------------------------------------------------------------
        private void ChooseCities(out City playerCity, out City enemyCity)
        {
            Random rand = new Random();
            int cityCount = map.cities.Count;

            playerCity = null;
            enemyCity = null;

            bool citiesPlaced = false;

            do
            {
                int playerCityID = rand.Next(cityCount) + 1;
                int enemyCityID = rand.Next(cityCount) + 1;

                // If either city ID is out of bounds, try again
                if (playerCityID >= cityCount || enemyCityID >= cityCount)
                {
                    Debug.WriteLine("City placement: City ID out of bounds: player {0}, enemy {1}, bound {2}", playerCityID, enemyCityID, cityCount);
                    continue;
                }

                // If two cities are the same try again
                if (playerCityID == enemyCityID)
                {
                    Debug.WriteLine("City placement: Same city chosen");
                    continue;
                }

                // Get the two cities from the collection
                playerCity = map.cities[playerCityID];
                enemyCity = map.cities[enemyCityID];

                // If either city is an island, try again
                if (map.SeaFrontage(playerCity.x, playerCity.y) == 8 || map.SeaFrontage(enemyCity.x, enemyCity.y) == 8)
                {
                    Debug.WriteLine("City placement: City is an island");
                    continue;
                }

                // If either land mass has no ports, try again
                if (
                        map.bodies[map.Location(playerCity.x, playerCity.y).body].portCount == 0 ||
                        map.bodies[map.Location(enemyCity.x, enemyCity.y).body].portCount == 0
                    )
                {
                    Debug.WriteLine("City placement: City land mass has no ports");
                    continue;
                }

                // If the player land mass has more cities than the enemy land mass, swap!
                if (
                       map.bodies[map.Location(playerCity.x, playerCity.y).body].cityCount >
                       map.bodies[map.Location(enemyCity.x, enemyCity.y).body].cityCount
                    )
                {
                    Debug.WriteLine("City placement: Donating better city to enemy");
                    City swapCity = enemyCity;
                    enemyCity = playerCity;
                    playerCity = swapCity;
                }

                // Determine land-locked status of cities
                playerCity.landLocked = (this.map.SeaFrontage(playerCity.x, playerCity.y) == 0);
                enemyCity.landLocked = (this.map.SeaFrontage(enemyCity.x, enemyCity.y) == 0);

                // Set alignment of the two chosen cities
                playerCity.alignment = Alignment.Player;
                enemyCity.alignment = Alignment.Enemy;
                citiesPlaced = true;
            } while (!citiesPlaced);
        }

        //---------------------------------------------------------------------
        // ChooseCityProduction
        // ====================
        //
        // Ask the player to choose production for a city
        //---------------------------------------------------------------------
        public void ChooseCityProduction(City city)
        {
            UnitType production = presentation.ChooseCityProduction(playerMap, city);
            city.BeginProduction(production, GameDate);
        }

        //---------------------------------------------------------------------
        // Combat
        // ======
        //
        // Resolve combat between two units
        //---------------------------------------------------------------------
        private Unit Combat(Unit attacker, Unit defender, int x, int y)
        {
            if (defender.alignment == Alignment.Player)
            {
                presentation.DisplayMessage($"Your {defender.type.ToString()} at ({x},{y}) is under attack");
            }

            Random rand = new Random();
            Unit victor = null;

            do
            {
                int attackerRoll = rand.Next(1, 10);
                int defenderRoll = rand.Next(1, 10);

                // Attacker wins this round
                if (attackerRoll > defenderRoll)
                {
                    defender.hits -= attacker.damage;
                }
                // Defender wins this round
                else if (defenderRoll > attackerRoll)
                {
                    attacker.hits -= defender.damage;
                }

                // Defender destroyed
                if (defender.hits <= 0)
                {
                    defender.Destroyed = true;
                    victor = attacker;

                    if (defender.alignment == Alignment.Player)
                    {
                        presentation.DisplayMessage($"Your {defender.type.ToString()} at ({x},{y}) was destroyed");
                    }
                }

                // Attacker destroyed
                if (attacker.hits <= 0)
                {
                    attacker.Destroyed = true;
                    victor = defender;

                    if (attacker.alignment == Alignment.Player)
                    {
                        presentation.DisplayMessage($"Your {attacker.type.ToString()} at ({x},{y}) was victorious. {attacker.hits} hits remaining.");
                    }
                }

            } while (victor is null);

            return victor;
        }

        //---------------------------------------------------------------------
        // Create
        // ======
        //
        // Create a new game
        //---------------------------------------------------------------------
        public bool Create()
        {
            bool create = true;

            map = MapGenerator.Generate();

            if (map != null)
            {
                // Place the two starting cities
                ChooseCities(out playerCity, out City enemyCity);

                // Create the two players' maps
                InitialiseMaps();

                UpdatePlayerMap(playerCity.x, playerCity.y);
                UpdateEnemyMap(enemyCity.x, enemyCity.y);

                // Set enemy city to produce armies
                enemyCity.BeginProduction(UnitType.Army, GameDate);
            }

            return create;
        }

        //---------------------------------------------------------------------
        // DestroyEnemyUnits
        // =================
        //
        // Destroy unit objects that have been marked as removed from the game
        //---------------------------------------------------------------------
        private void DestroyEnemyUnits()
        {
            // Look for units that have just been destroyed
            var subset = from unit in units.Values
                         where unit.Destroyed && unit.alignment == Alignment.Enemy
                         orderby unit.Id
                         select unit;

            Collection<int> destroyed = new Collection<int>();

            foreach (Unit unit in subset)
            {
                destroyed.Add(unit.Id);
            }

            // Removed destroyed units
            foreach (int id in destroyed)
            {
                Unit unit = units[id];

                int x = unit.x;
                int y = unit.y;

                // Update the map around this location
                map.Location(x, y).occupant = null;
                UpdateMap(enemyMap, x, y);
                presentation.UpdateAdjacent(enemyMap, x, y);
                units.Remove(id);
                unit = null;
            }
        }

        //---------------------------------------------------------------------
        // DestroyUnits
        // ============
        //
        // Destroy unit objects that have been marked as removed from the game
        //---------------------------------------------------------------------
        private void DestroyUnits()
        {
            // Look for units that have just been destroyed
            var subset = from unit in units.Values
                         where unit.Destroyed && unit.alignment == Alignment.Player
                         orderby unit.Id
                         select unit;

            Collection<int> destroyed = new Collection<int>();

            foreach (Unit unit in subset)
            {
                destroyed.Add(unit.Id);
            }

            // Removed destroyed units
            foreach (int id in destroyed)
            {
                Unit unit = units[id];

                int x = unit.x;
                int y = unit.y;

                // Update the map around this location
                map.Location(x, y).occupant = null;
                UpdateMap(playerMap, x, y);
                presentation.UpdateAdjacent(playerMap, x, y);
                units.Remove(id);
                unit = null;
            }
        }

        //---------------------------------------------------------------------
        // EnemyMovement
        // ==============
        //
        // Allow the enemy to move its units
        //---------------------------------------------------------------------
        private void EnemyMovement(UnitType unitType)
        {
            // Player movement
            foreach (Unit unit in units.Values)
            {
                if (unit.alignment == Alignment.Enemy && unit.type == unitType)
                {
                    ShowUnitStatus(unit);

                    switch (unit.type)
                    {
                        case UnitType.Army:
                            MoveEnemyUnit((Army)unit);
                            break;
                        case UnitType.Fighter:
                            MoveEnemyUnit((Fighter)unit);
                            break;
                        case UnitType.Helicopter:
                            MoveEnemyUnit((Helicopter)unit);
                            break;
                        default:
                            MoveEnemyUnit(unit);
                            break;
                    }
                }
            }
        }

        //---------------------------------------------------------------------
        // EnemyProduction
        // ==============
        //
        // Add units that have been constructed
        //---------------------------------------------------------------------
        private void EnemyProduction()
        {
            foreach (City city in map.cities.Values)
            {
                if (city.alignment == Alignment.Enemy && city.ProductionComplete(GameDate))
                {
                    Unit unit = new Unit();

                    switch (city.Production)
                    {
                        case UnitType.Army:
                            unit = new Army(unitID);
                            break;

                        case UnitType.Fighter:
                            unit = new Fighter(unitID);
                            break;

                        case UnitType.Submarine:
                            unit = new Submarine(unitID);
                            break;

                        case UnitType.Destroyer:
                            unit = new Destroyer(unitID);
                            break;

                        case UnitType.Cruiser:
                            unit = new Cruiser(unitID);
                            break;

                        case UnitType.Battleship:
                            unit = new Battleship(unitID);
                            break;

                        case UnitType.AircraftCarrier:
                            unit = new AircraftCarrier(unitID);
                            break;

                        case UnitType.AssaultShip:
                            unit = new AssaultShip(unitID);
                            break;

                        case UnitType.TroopTransport:
                            unit = new TroopTransport(unitID);
                            break;

                        case UnitType.Helicopter:
                            unit = new Helicopter(unitID);
                            break;
                    }

                    unit.x = city.x;
                    unit.y = city.y;
                    unit.alignment = Alignment.Enemy;

                    units.Add(unitID, unit);
                    unitID++;
                    city.occupants.Add(unit);
                    city.BeginProduction(city.Production, GameDate);
                }
            }
        }

        //---------------------------------------------------------------------
        // GetAction
        // =========
        //
        // Get an action to be performed by a unit
        //---------------------------------------------------------------------
        private ActionType GetAction(Unit unit, int dx, int dy)
        {
            ActionType actionType;

            if (map.EdgeCheck(unit.x, unit.y, dx, dy))
            {
                Terrain destination = map.Location(unit.x + dx, unit.y + dy);

                switch (unit.type)
                {
                    case UnitType.Army:
                        actionType = GetUnitAction((Army)unit, destination);
                        break;

                    case UnitType.Fighter:
                        actionType = GetUnitAction((Fighter)unit, destination);
                        break;

                    case UnitType.Helicopter:
                        actionType = GetUnitAction((Helicopter)unit, destination);
                        break;

                    default:
                        actionType = GetUnitAction(unit, destination);
                        break;
                }
            }
            else
            {
                actionType = ActionType.Invalid;
            }

            return actionType;
        }

        //---------------------------------------------------------------------
        // GetDeltas
        // =========
        //
        // Returns the delta-x and delta-y required by the specified movement
        // direction
        //---------------------------------------------------------------------
        private void GetDeltas(OrderType orderType, out int dx, out int dy)
        {
            dx = 0;
            dy = 0;

            switch (orderType)
            {
                case OrderType.MoveNorth:
                    dy = -1;
                    break;
                case OrderType.MoveNorthEast:
                    dx = 1;
                    dy = -1;
                    break;
                case OrderType.MoveEast:
                    dx = 1;
                    break;
                case OrderType.MoveSouthEast:
                    dx = 1;
                    dy = 1;
                    break;
                case OrderType.MoveSouth:
                    dy = 1;
                    break;
                case OrderType.MoveSouthWest:
                    dx = -1;
                    dy = 1;
                    break;
                case OrderType.MoveWest:
                    dx = -1;
                    break;
                case OrderType.MoveNorthWest:
                    dx = -1;
                    dy = -1;
                    break;
            }
        }

        //---------------------------------------------------------------------
        // GetUnitAction
        // =============
        //
        // Identifies the action being performed for army units
        //---------------------------------------------------------------------
        private ActionType GetUnitAction(Army army, Terrain destination)
        {
            ActionType actionType = ActionType.Invalid;

            // If underlying terrain at the destination is sea
            if (destination.type == TerrainType.Sea)
            {
                // Can't move into open water
                if (destination.occupant == null)
                    actionType = ActionType.Invalid;

                // If we're on a troop transport or helicopter
                else if (army.Embarked)
                {
                    // We're on a troop transport
                    if (((Unit)army.EmbarkedOn).type == UnitType.TroopTransport)
                    {
                        // If we're already on this transport, then we're staying onboard
                        if (destination.occupant == army.EmbarkedOn)
                            actionType = ActionType.None;

                        // If the occupant of the destination is an enemy, attack it
                        else if (destination.occupant.alignment == Alignment.Enemy)
                            actionType = ActionType.AttackUnit;

                        // If it is a friendly troop transport, can we embark?
                        else if (destination.occupant.type == UnitType.TroopTransport)
                        {
                            // If there's space on the transport...
                            if (!((ITransporter)destination.occupant).IsFull)
                                actionType = ActionType.ArmyTransfer;
                            else
                                actionType = ActionType.Invalid;
                        }

                        // Don't allow attacking own vessels
                        else
                            actionType = ActionType.Invalid;
                    }

                    // We're in a helicopter, we can't attack or board another ship
                    else
                    {
                        // If we're already on this helicopter, then we're staying onboard
                        if (destination.occupant == army.EmbarkedOn)
                            actionType = ActionType.None;

                        // If we're on a helicopter that is aboard an assualt ship.
                        else if (((Helicopter)army.EmbarkedOn).EmbarkedOn == destination.occupant)
                            actionType = ActionType.None;

                        else
                            actionType = ActionType.Invalid;
                    }
                }

                // We're not embarked on anything
                else
                {
                    // If the occupant of the destination is an enemy, attack it
                    if (destination.occupant.alignment == Alignment.Enemy)
                        actionType = ActionType.AttackUnit;

                    // If it is a friendly troop transport, can we embark?
                    else if (destination.occupant.type == UnitType.TroopTransport)
                    {
                        // If there's space on the transport...
                        if (!((ITransporter)destination.occupant).IsFull)
                            actionType = ActionType.EmbarkTransport;
                        else
                            actionType = ActionType.Invalid;
                    }

                    // Don't allow attacking own units
                    else
                        actionType = ActionType.Invalid;
                }
            }

            // If destination is another city
            else if (destination.type == TerrainType.City)
            {
                // If we're in a transport, and that transport is docked, or landed, in the city
                // then we can remain on board the transport
                if (army.Embarked && destination.city.occupants.Contains((Unit)army.EmbarkedOn))
                    actionType = ActionType.None;

                // Can't enter our cities
                else if (destination.city.alignment == Alignment.Player)
                    actionType = ActionType.Invalid;

                // But other cities can be attacked
                else
                    actionType = ActionType.AttackCity;
            }

            // If destination is land...
            else if (destination.type == TerrainType.Land)
            {
                // If land is empty, just move
                if (destination.occupant == null)
                    actionType = ActionType.Move;

                // If it is this army standing still 
                // out in the open 
                // on board a helicopter
                // on board a helicopter that is on board an assault ship
                else if (
                            destination.occupant == army ||
                            army.EmbarkedOn == destination.occupant ||
                            (army.EmbarkedOn is Helicopter helicopter && helicopter.EmbarkedOn == destination.occupant)
                        )
                    actionType = ActionType.None;

                else
                {
                    // Friendly forces...
                    if (destination.occupant.alignment == Alignment.Player)
                    {
                        // It's a helicopter with room to spare
                        if (destination.occupant.type == UnitType.Helicopter && !((ITransporter)destination.occupant).IsFull)
                        {
                            actionType = ActionType.EmbarkHelicopter;
                        }

                        // Don't attack friendly forces
                        else
                            actionType = ActionType.Invalid;
                    }

                    // Otherwise attack enemy forces
                    else
                        actionType = ActionType.AttackUnit;
                }
            }

            return actionType;
        }

        //---------------------------------------------------------------------
        // GetUnitAction
        // =============
        //
        // Identifies the action being performed for fighter units
        //---------------------------------------------------------------------
        private ActionType GetUnitAction(Fighter fighter, Terrain destination)
        {
            ActionType actionType;

            // If destination is sea
            if (destination.type == TerrainType.Sea)
            {
                // Overwater flight
                if (destination.occupant == null)
                    actionType = ActionType.Move;
                else
                {
                    // If the occupant is an enemy vessel, attack it
                    if (destination.occupant.alignment == Alignment.Enemy)
                        actionType = ActionType.AttackUnit;

                    // If it is a friendly aircraft carrier, land on board
                    else if (destination.occupant.type == UnitType.AircraftCarrier && !((AircraftCarrier)destination.occupant).IsFull)
                        // If we're already on a carrier, perform a transfer
                        if (fighter.Embarked)
                            actionType = ActionType.AircraftTransfer;
                        else
                            actionType = ActionType.LandVessel;

                    // Don't allow attacking own vessels
                    else
                        actionType = ActionType.Invalid;
                }
            }

            // If destination is a city
            else if (destination.type == TerrainType.City)
            {
                // If it is one of our cities, land there
                if (destination.city.alignment == Alignment.Player)
                    actionType = ActionType.LandAirbase;

                // Otherwise, don't enter city
                else
                    actionType = ActionType.Invalid;
            }

            // Destination is land
            else
            {
                // If empty land, continue flight
                if (destination.occupant == null)
                    actionType = ActionType.Move;

                // If the land is not empty
                else
                {
                    // Don't attack our own forces
                    if (destination.occupant.alignment == Alignment.Player)
                        actionType = ActionType.Invalid;

                    // Attack enemy units
                    else
                        actionType = ActionType.AttackUnit;
                }
            }

            return actionType;
        }

        //---------------------------------------------------------------------
        // GetUnitAction
        // =============
        //
        // Identifies the action being performed for helicopter units
        //---------------------------------------------------------------------
        private ActionType GetUnitAction(Helicopter helicopter, Terrain destination)
        {
            ActionType actionType;

            // If destination is sea
            if (destination.type == TerrainType.Sea)
            {
                // If we're loitering
                if (destination.occupant == helicopter || (helicopter.Embarked && destination.occupant == helicopter.EmbarkedOn))
                    actionType = ActionType.None;

                // Overwater flight
                else if (destination.occupant == null)
                    actionType = ActionType.Move;

                else
                {
                    // If the occupant is an enemy vessel, attack it
                    if (destination.occupant.alignment == Alignment.Enemy)
                        actionType = ActionType.AttackUnit;

                    // If it is a friendly assault ship, land on board
                    else if (destination.occupant.type == UnitType.AssaultShip && !((AssaultShip)destination.occupant).IsFull)
                        // If we're already embarked, perform a transfer
                        if (helicopter.Embarked)
                            actionType = ActionType.AircraftTransfer;
                        else
                            actionType = ActionType.LandVessel;

                    // Don't allow attacking own vessels
                    else
                        actionType = ActionType.Invalid;
                }
            }

            // If destination is a city
            else if (destination.type == TerrainType.City)
            {
                // If it is one of our cities, land there
                if (destination.city.alignment == Alignment.Player)
                    actionType = ActionType.LandAirbase;

                // Otherwise, don't enter city
                else
                    actionType = ActionType.Invalid;
            }

            // Destination is land
            else
            {
                // If empty land, continue flight
                if (destination.occupant == null)
                    actionType = ActionType.Move;

                // If the land is not empty
                else
                {
                    // If the terrain occupant is this piece, then we're loitering
                    if (destination.occupant == helicopter)
                        actionType = ActionType.None;

                    // Don't attack our own forces
                    else if (destination.occupant.alignment == Alignment.Player)
                        actionType = ActionType.Invalid;

                    // Attack enemy units
                    else
                        actionType = ActionType.AttackUnit;
                }
            }

            return actionType;
        }

        //---------------------------------------------------------------------
        // GetUnitAction
        // =============
        //
        // Identifies the action being performed for other units
        //---------------------------------------------------------------------
        private ActionType GetUnitAction(Unit unit, Terrain destination)
        {
            ActionType actionType;

            // If destination is sea
            if (destination.type == TerrainType.Sea)
            {
                // Continue sailing
                if (destination.occupant == null)
                    actionType = ActionType.Move;

                // If we're staying put.
                else if (destination.occupant == unit)
                    actionType = ActionType.None;

                else
                {
                    // If the occupant is an enemy vessel, attack it
                    if (destination.occupant.alignment == Alignment.Enemy)
                        actionType = ActionType.AttackUnit;

                    // Don't allow attacking own vessels
                    else
                        actionType = ActionType.Invalid;
                }
            }

            // If destination is a city
            else if (destination.type == TerrainType.City)
            {
                // If it is one of our cities, dock there
                if (destination.city.alignment == Alignment.Player)
                    actionType = ActionType.Dock;

                // Otherwise, don't attack city
                else
                    actionType = ActionType.Invalid;
            }
            // Destination is land
            else
                actionType = ActionType.Invalid;

            return actionType;
        }

        //---------------------------------------------------------------------
        // InitialiseMaps
        // ==============
        //
        // Create the blank player and enemy maps
        //---------------------------------------------------------------------
        public void InitialiseMaps()
        {
            playerMap = new Map(map.width, map.height, TerrainType.None);
            enemyMap = new Map(map.width, map.height, TerrainType.None);
        }

        //---------------------------------------------------------------------
        // MoveUnit
        // ========
        //
        // Allows the player to move an army
        //---------------------------------------------------------------------
        private void MoveUnit(Army army)
        {
            Terrain location = map.Location(army.x, army.y);

            // Don't ask for orders if either of the following is true...
            //  •   the army is in sentry mode
            //  •   the army is at sea, and there is no adjecent terrain
            //  •   the army is embarked on a helo, and the helo is overwater
            //      (this also works when the helo is itself embrked
            //
            if (
                    !army.Sentry
                && !(army.Embarked && playerMap.SeaFrontage(army.x, army.y) == 8)
                && !(army.EmbarkedOn is Helicopter && location.type == TerrainType.Sea)
                )
            {
                ActionType actionType = ActionType.Invalid;
                int dx = 0;
                int dy = 0;
                int xDestination;
                int yDestination;

                UpdatePlayerMap(army);

                while (actionType == ActionType.Invalid)
                {
                    OrderType orders = presentation.GetOrders(army.x, army.y);

                    // Orders are to stay at current location
                    // We can do this if the army is embarked, if the current location is not a city, or if
                    // the army is out in the open
                    if (orders == OrderType.None && location.type != TerrainType.City)
                    {
                        actionType = ActionType.None;
                    }

                    // Orders were to enable sentry mode
                    // This can be done of the army is not embarked, and the army is not in a city
                    else if (orders == OrderType.Sentry && !(army.Embarked || location.type == TerrainType.City))
                    {
                        actionType = ActionType.None;
                        army.Sentry = true;
                    }

                    // Otherwise orders are to move
                    // If movement puts army on map edge then Invalid action type is returned
                    GetDeltas(orders, out dx, out dy);
                    actionType = GetAction(army, dx, dy);

                    if (actionType == ActionType.Invalid)
                    {
                        presentation.Alert();
                    }
                }

                // Orders are valid
                // Make sure the result of the orders is valid
                xDestination = army.x + dx;
                yDestination = army.y + dy;

                Terrain destination = map.Location(xDestination, yDestination);

                // What action is being taken?
                switch (actionType)
                {
                    case ActionType.AttackUnit:
                        // If the army is attacking from a transport,get off
                        if (army.Embarked)
                        {
                            army.Disembark();
                        }

                        // The army vacates the original location
                        else if (location.occupant == army)
                        {
                            location.occupant = null;
                        }

                        // Winner takes the destination
                        // Then update the maps around the two locations
                        destination.occupant = Combat(army, destination.occupant, xDestination, yDestination);
                        UpdatePlayerMap(army.x, army.y);
                        UpdatePlayerMap(xDestination, yDestination);
                        UpdateEnemyMap(army.x, army.y);
                        UpdateEnemyMap(xDestination, yDestination);
                        break;

                    case ActionType.EmbarkTransport:
                    case ActionType.EmbarkHelicopter:
                        {
                            //
                            // Get the helicopter or troop transporter
                            // Add the army to its occupants
                            // End the army's turn by removing remaining movement
                            // Remove the army from its last location
                            //
                            ITransporter transporter = (ITransporter)map.Location(xDestination, yDestination).occupant;

                            if (!transporter.IsFull)
                            {
                                army.Embark(transporter);
                            }

                            location.occupant = null;
                        }
                        break;

                    // Simple move
                    case ActionType.Move:
                        {
                            destination.occupant = army;

                            // If moving out of a city, remove from the list of occupants
                            if (location.type == TerrainType.City)
                            {
                                location.city.occupants.Remove(army);
                            }

                            // If disembarking, remove from the list of occupants
                            else if (army.Embarked)
                            {
                                army.Disembark();
                            }

                            // If this was a simple move
                            if (location.occupant == army)
                            {
                                location.occupant = null;
                            }
                        }
                        break;

                    // Stay where we are.
                    case ActionType.None:
                        break;

                    case ActionType.AttackCity:
                        {
                            // Attack the city
                            City city = destination.city;
                            Alignment alignment = city.alignment;

                            AttackCity(army, city);

                            // Whatever the outcome the army is removed from the field
                            army.Destroyed = true;
                            if (location.occupant == army)
                            {
                                location.occupant = null;
                            }
                            else
                            {
                                ((ITransporter)location.occupant).Passengers.Remove(army);
                            }

                            switch (alignment)
                            {
                                case Alignment.Player:
                                    UpdatePlayerMap(city.x, city.y);
                                    break;
                                case Alignment.Enemy:
                                    UpdateEnemyMap(city.x, city.y);
                                    break;
                            }
                        }
                        break;

                    case ActionType.ArmyTransfer:
                        {
                            // Remove the army from the transport at the current location
                            // Add the army to the transport at the destination
                            army.Disembark();
                            army.Embark((TroopTransport)destination.occupant);
                        }
                        break;

                    case ActionType.Invalid:
                        presentation.Alert();
                        break;
                }

                // We have emerged with a valid action
                // Update army's location
                // Update master and player maps
                army.x = xDestination;
                army.y = yDestination;
                UpdateMap(playerMap, xDestination, yDestination);
                presentation.UpdateAdjacent(playerMap, xDestination, yDestination);
            }
        }

        //---------------------------------------------------------------------
        // MoveUnit
        // ========
        //
        // Allows the player to move a fighter
        //---------------------------------------------------------------------
        private void MoveUnit(Fighter fighter)
        {
            Terrain location = map.Location(fighter.x, fighter.y);
            Terrain destination;
            ActionType actionType;

            int dx = 0;
            int dy = 0;
            int xDestination;
            int yDestination;
            int movement = Math.Min(fighter.Movement, fighter.Range);

            // If the fighter exhausted its fuel last turn, mark it as destroyed
            if (movement <= 0)
            {
                fighter.Destroyed = true;
            }

            while (movement > 0)
            {
                do
                {
                    // Get some orders
                    OrderType orders = presentation.GetOrders(fighter.x, fighter.y);

                    // Orders are to stay at current location
                    // We can do this if the fighter is embarked, or if the current location is not a city
                    if (orders == OrderType.None && (fighter.Embarked || location.type != TerrainType.City))
                    {
                        actionType = ActionType.None;
                    }

                    // Orders were to enable sentry mode - Not permitted for fighters
                    else if (orders == OrderType.Sentry)
                    {
                        actionType = ActionType.Invalid;
                    }

                    // Otherwise orders are to move - if the movement would put the fighter
                    // on the edge of the map, then the Invalid action type is returned
                    else
                    {
                        GetDeltas(orders, out dx, out dy);
                        actionType = GetAction(fighter, dx, dy);
                    }

                    if (actionType == ActionType.Invalid)
                    {
                        presentation.Alert();
                    }
                } while (actionType == ActionType.Invalid);

                // Orders are valid
                // Make sure the result of the orders is valid
                xDestination = fighter.x + dx;
                yDestination = fighter.y + dy;
                destination = map.Location(xDestination, yDestination);

                // What action is being taken?
                switch (actionType)
                {
                    case ActionType.AttackUnit:
                        Combat(fighter, destination.occupant, xDestination, yDestination);
                        break;

                    // Simple move
                    case ActionType.Move:
                        {
                            // If moving out of a city, remove from the list of occupants
                            if (location.type == TerrainType.City)
                            {
                                location.city.occupants.Remove(fighter);
                            }

                            // If disembarking a carrier, remove from the list of occupants
                            else if (fighter.Embarked)
                            {
                                ((AircraftCarrier)location.occupant).Passengers.Remove(fighter);
                                fighter.Disembark();
                            }

                            // If this was a simple move
                            if (location.occupant == fighter)
                            {
                                location.occupant = null;
                            }

                            // Put the fighter on the map at the new location
                            // Reduce avaiable range
                            // Reduce remaining movement
                            // Set source location to destination now that we've moved
                            destination.occupant = fighter;
                            fighter.ConsumeFuel();
                            movement--;
                            location = destination;
                        }
                        break;

                    // Land fighter in city
                    case ActionType.LandAirbase:
                        {
                            //
                            // Add the fighter to the city's occupants
                            // Refuel the fighter
                            // Remove the fighter from its last location
                            // End the fighter's turn by removing remaining movement
                            //
                            destination.city.occupants.Add(fighter);
                            fighter.Refuel();
                            location.occupant = null;
                            movement = 0;
                        }
                        break;

                    case ActionType.LandVessel:
                        {
                            //
                            // Add the aircraft to the carrier's occupants
                            // Refuel the aircraft
                            // Remove the aircraft from its last location
                            // End the aircraft's turn by removing remaining movement
                            //
                            fighter.Embark((AircraftCarrier)destination.occupant);
                            fighter.Refuel();
                            location.occupant = null;
                            movement = 0;
                        }
                        break;

                    case ActionType.AircraftTransfer:
                        {
                            // Remove the fighter from the carrier at the current location
                            // Add the fighter to the carrier at the destination
                            // Refuel the fighter
                            // End movement
                            fighter.Disembark();
                            fighter.Embark((AircraftCarrier)destination.occupant);
                            fighter.Refuel();
                            movement = 0;
                        }
                        break;

                    // Stay where we are.
                    case ActionType.None:
                        // Consume fuel for remaining movement
                        // End movement
                        if (!fighter.Embarked)
                        {
                            fighter.ConsumeFuel(movement);
                        }

                        movement = 0;
                        break;
                }

                // We have performed an action
                // Update fighters's location
                // Update master and player maps
                fighter.x = xDestination;
                fighter.y = yDestination;
                UpdateMap(playerMap, xDestination, yDestination);
                presentation.UpdateAdjacent(playerMap, xDestination, yDestination);
            }
        }

        //---------------------------------------------------------------------
        // MoveUnit
        // ========
        //
        // Allows the player to move a helicopter
        //---------------------------------------------------------------------
        private void MoveUnit(Helicopter helicopter)
        {
            // Don't ask for orders if the helicopter is in fill mode
            Terrain location = map.Location(helicopter.x, helicopter.y);
            ActionType actionType;

            int dx = 0;
            int dy = 0;
            int xDestination;
            int yDestination;
            int movement = Math.Min(helicopter.Movement, helicopter.Range);

            // If we're waiting for passengers, don't bother asking for orders
            if (helicopter.Fill)
            {
                movement = 0;
            }

            // If the helicopter exhausted its fuel last turn, mark it as destroyed
            if (movement <= 0 || helicopter.Fill)
            {
                helicopter.Destroyed = true;
            }

            while (movement > 0)
            {
                do
                {
                    OrderType orders = presentation.GetOrders(helicopter.x, helicopter.y);

                    // Orders are to stay at current location
                    // We can do this if the helicopter is embarked, or if the current location is not a city
                    if (orders == OrderType.None && (helicopter.Embarked || location.type != TerrainType.City))
                    {
                        actionType = ActionType.None;
                    }

                    // Orders were to enable sentry mode - Not permitted for helicopters
                    else if (orders == OrderType.Sentry)
                    {
                        actionType = ActionType.Invalid;
                    }

                    // Sleep until full... can do this only if we're on the ground
                    else if (orders == OrderType.Fill)
                    {
                        if (!helicopter.Embarked && location.occupant == helicopter && location.type == TerrainType.Land)
                        {
                            actionType = ActionType.Fill;
                        }
                        else
                        {
                            actionType = ActionType.Invalid;
                        }
                    }

                    // Otherwise orders are to move.
                    // If move would put helicopter on the edge of the map, then invalid action type returned
                    else
                    {
                        GetDeltas(orders, out dx, out dy);
                        actionType = GetAction(helicopter, dx, dy);
                    }

                    if (actionType == ActionType.Invalid)
                    {
                        presentation.Alert();
                    }
                } while (actionType == ActionType.Invalid);

                // Orders are valid
                // Make sure the result of the orders is valid
                xDestination = helicopter.x + dx;
                yDestination = helicopter.y + dy;

                Terrain destination = map.Location(xDestination, yDestination);

                // What action is being taken?
                switch (actionType)
                {
                    case ActionType.AttackUnit:
                        Combat(helicopter, destination.occupant, xDestination, yDestination);
                        break;

                    // Simple move
                    case ActionType.Move:
                        {
                            // If moving out of a city, remove from the list of occupants
                            if (location.type == TerrainType.City)
                            {
                                location.city.occupants.Remove(helicopter);
                            }

                            // If disembarking an assault ship, remove from the list of occupants
                            else if (helicopter.Embarked)
                            {
                                helicopter.Disembark();
                            }

                            // If this was a simple move
                            if (location.occupant == helicopter)
                            {
                                location.occupant = null;
                            }

                            // Put the helicopter on the map at the new location
                            // Bring passengers
                            // Reduce avaiable range
                            // Reduce remaining movement
                            // Change source location now that we've moved
                            destination.occupant = helicopter;
                            helicopter.ConsumeFuel();
                            movement--;
                            location = destination;
                        }
                        break;

                    // Land helicopter in city
                    case ActionType.LandAirbase:
                        {
                            //
                            // Add the helicopter to the city's occupants
                            // Bring passengers
                            // Refuel the helicopter
                            // Remove the helicopter from its last location
                            // End the helicopter's turn by removing remaining movement
                            //
                            destination.city.occupants.Add(helicopter);
                            helicopter.Refuel();
                            location.occupant = null;
                            movement = 0;
                        }
                        break;

                    case ActionType.LandVessel:
                        {
                            //
                            // Add the helicopter to the assault ship's occupants
                            // Refuel the helicopter
                            // Remove the helicopter from its last location
                            // End the helicopter's turn by removing remaining movement
                            //
                            helicopter.Embark((AssaultShip)destination.occupant);
                            helicopter.Refuel();
                            location.occupant = null;
                            movement = 0;
                        }
                        break;

                    case ActionType.AircraftTransfer:
                        {
                            // Remove the helicopter from the carrier at the current location
                            // Add the helicopter to the carrier at the destination
                            // Change the vessel that the helicopter is embarked on
                            // Bring passengers
                            // Refuel the helicopter
                            // End movement
                            helicopter.Disembark();
                            helicopter.Embark((AssaultShip)destination.occupant);
                            helicopter.Refuel();
                            movement = 0;
                        }
                        break;

                    case ActionType.Fill:
                        if (!helicopter.Embarked && location.occupant == helicopter && location.type == TerrainType.Land)
                        {
                            helicopter.Fill = true;
                            movement = 0;
                        }
                        break;

                    // Stay where we are.
                    case ActionType.None:
                        // Consume fuel for remaining movement
                        // End movement
                        if (!helicopter.Embarked)
                        {
                            helicopter.ConsumeFuel(movement);
                        }
                        movement = 0;
                        break;
                }

                // We have performed an action
                // Update helicopters's location
                // Update master and player maps
                helicopter.x = xDestination;
                helicopter.y = yDestination;
                helicopter.ConveyPassengers();
                UpdateMap(playerMap, xDestination, yDestination);
                presentation.UpdateAdjacent(playerMap, xDestination, yDestination);
            }
        }

        //---------------------------------------------------------------------
        // MoveUnit
        // ========
        //
        // Allows the player to move other units (all ships)
        //---------------------------------------------------------------------
        private void MoveUnit(Unit unit)
        {
            Terrain location = map.Location(unit.x, unit.y);
            int movement = unit.Movement;
            ActionType actionType;
            int dx;
            int dy;
            int xDestination;
            int yDestination;

            // Don't ask for orders if the unit is in sentry mode
            // Don't ask for orders if a transport is in fill mode
            if ((unit is ISentry && ((ISentry)unit).Sentry) || (unit is ITransporter && ((ITransporter)unit).Fill))
            {
                movement = 0;
            }

            while (movement > 0)
            {
                do
                {
                    dx = 0;
                    dy = 0;

                    OrderType orders = presentation.GetOrders(unit.x, unit.y);

                    // Orders are to stay at current location
                    if (orders == OrderType.None)
                    {
                        actionType = ActionType.None;
                    }

                    // Orders were to enable sentry mode
                    // This can be done if the unit is not in a city
                    else if (orders == OrderType.Sentry && location.type != TerrainType.City)
                    {
                        actionType = ActionType.None;
                        ((ISentry)unit).Sentry = true;
                    }

                    // Sleep until full... can do this only if we're at sea
                    else if (orders == OrderType.Fill)
                    {
                        if (location.type == TerrainType.Sea)
                        {
                            actionType = ActionType.Fill;
                        }
                        else
                        {
                            actionType = ActionType.Invalid;
                        }
                    }

                    // Otherwise orders are to move.
                    // If movement would put unit on map edge, invalid action type is returned
                    else
                    {
                        GetDeltas(orders, out dx, out dy);
                        actionType = GetAction(unit, dx, dy);
                    }

                    if (actionType == ActionType.Invalid)
                    {
                        presentation.Alert();
                    }
                } while (actionType == ActionType.Invalid);

                // Orders are valid
                // Make sure the result of the orders is valid
                xDestination = unit.x + dx;
                yDestination = unit.y + dy;

                Terrain destination = map.Location(xDestination, yDestination);

                // What action is being taken?
                switch (actionType)
                {
                    case ActionType.AttackUnit:
                        Combat(unit, destination.occupant, xDestination, yDestination);
                        break;

                    // Simple move
                    case ActionType.Move:
                        {
                            // If moving out of a city, remove from the list of occupants
                            if (location.type == TerrainType.City)
                            {
                                location.city.occupants.Remove(unit);
                            }

                            // If this was a simple move
                            else if (location.occupant == unit)
                            {
                                location.occupant = null;
                            }

                            // Reduce available movement
                            // Change source location now that we've moved
                            movement--;
                            location = destination;
                            destination.occupant = unit;
                        }
                        break;

                    // Enter friendly city
                    case ActionType.Dock:
                        {
                            //
                            // Add the vessel to the city's occupants
                            // End the vessel's turn by removing remaining movement
                            // Remove the vessel from its last location
                            //
                            destination.city.occupants.Add(unit);
                            location.occupant = null;
                            movement = 0;
                        }
                        break;

                    // Sleep until full
                    case ActionType.Fill:
                        if (unit is ITransporter transporter)
                        {
                            transporter.Fill = true;
                            movement = 0;
                        }
                        break;

                    // Stay where we are.
                    case ActionType.None:
                        movement = 0;
                        break;

                    case ActionType.Invalid:
                        presentation.Alert();
                        break;
                }

                // We have emerged with a valid action
                // Update unit's location
                // Bring passengers
                // Update master and player maps
                unit.x = xDestination;
                unit.y = yDestination;

                if (unit is ITransporter)
                {
                    ((ITransporter)unit).ConveyPassengers();
                }

                UpdateMap(playerMap, xDestination, yDestination);
                presentation.UpdateAdjacent(playerMap, xDestination, yDestination);
            }
        }

        //---------------------------------------------------------------------
        // PerformTurn
        // ===========
        //
        // Perform moves and construction
        //---------------------------------------------------------------------
        public void PerformTurn()
        {

            // Player movement
            presentation.DisplayMap(playerMap);

            PlayerMovement(UnitType.TroopTransport);
            PlayerMovement(UnitType.AssaultShip);
            PlayerMovement(UnitType.AircraftCarrier);
            PlayerMovement(UnitType.Battleship);
            PlayerMovement(UnitType.Cruiser);
            PlayerMovement(UnitType.Submarine);
            PlayerMovement(UnitType.Destroyer);
            PlayerMovement(UnitType.Helicopter);
            PlayerMovement(UnitType.Army);
            PlayerMovement(UnitType.Fighter);

            // Player production
            PlayerProduction();

            // Destroy player units
            DestroyUnits();


            // Enemy movement
            presentation.DisplayMap(enemyMap);

            EnemyMovement(UnitType.TroopTransport);
            EnemyMovement(UnitType.AssaultShip);
            EnemyMovement(UnitType.AircraftCarrier);
            EnemyMovement(UnitType.Battleship);
            EnemyMovement(UnitType.Cruiser);
            EnemyMovement(UnitType.Submarine);
            EnemyMovement(UnitType.Destroyer);
            EnemyMovement(UnitType.Helicopter);
            EnemyMovement(UnitType.Army);
            EnemyMovement(UnitType.Fighter);

            // Enemy production
            EnemyProduction();

            // Destroy enemy units
            DestroyEnemyUnits();

            // Repair all cities
            RepairCities();

            // Check victory conditions

            // Increment game date
            GameDate++;
        }

        //---------------------------------------------------------------------
        // PlayerMovement
        // ==============
        //
        // Allow the player to move their units
        //---------------------------------------------------------------------
        private void PlayerMovement(UnitType unitType)
        {
            // Player movement
            foreach (Unit unit in units.Values)
            {
                if (unit.alignment == Alignment.Player && unit.type == unitType)
                {
                    ShowUnitStatus(unit);

                    switch (unit.type)
                    {
                        case UnitType.Army:
                            MoveUnit((Army)unit);
                            break;
                        case UnitType.Fighter:
                            MoveUnit((Fighter)unit);
                            break;
                        case UnitType.Helicopter:
                            MoveUnit((Helicopter)unit);
                            break;
                        default:
                            MoveUnit(unit);
                            break;
                    }
                }
            }
        }

        //---------------------------------------------------------------------
        // PlayerProduction
        // ================
        //
        // Add units that have been constructed
        //---------------------------------------------------------------------
        private void PlayerProduction()
        {
            foreach (City city in map.cities.Values)
            {
                if (city.alignment == Alignment.Player && city.ProductionComplete(GameDate))
                {
                    Unit unit = new Unit();

                    switch (city.Production)
                    {
                        case UnitType.Army:
                            unit = new Army(unitID);
                            break;

                        case UnitType.Fighter:
                            unit = new Fighter(unitID);
                            break;

                        case UnitType.Submarine:
                            unit = new Submarine(unitID);
                            break;

                        case UnitType.Destroyer:
                            unit = new Destroyer(unitID);
                            break;

                        case UnitType.Cruiser:
                            unit = new Cruiser(unitID);
                            break;

                        case UnitType.Battleship:
                            unit = new Battleship(unitID);
                            break;

                        case UnitType.AircraftCarrier:
                            unit = new AircraftCarrier(unitID);
                            break;

                        case UnitType.AssaultShip:
                            unit = new AssaultShip(unitID);
                            break;

                        case UnitType.TroopTransport:
                            unit = new TroopTransport(unitID);
                            break;

                        case UnitType.Helicopter:
                            unit = new Helicopter(unitID);
                            break;
                    }

                    unit.x = city.x;
                    unit.y = city.y;
                    unit.alignment = Alignment.Player;

                    units.Add(unitID, unit);
                    unitID++;
                    city.occupants.Add(unit);
                    city.BeginProduction(city.Production, GameDate);
                }
            }
        }

        //---------------------------------------------------------------------
        // RepairCities
        // ============
        //
        // Cities that have been attacked lose hit points. At the end of every
        // 5th turn a city can repair one point of damage
        //---------------------------------------------------------------------
        private void RepairCities()
        {
            if (GameDate % 5 == 0)
            {
                foreach (City city in map.cities.Values)
                {
                    city.Repair();
                }
            }
        }

        //---------------------------------------------------------------------
        // ShowUnitStatus
        // ==============
        //
        // Show details of the selected unit
        //---------------------------------------------------------------------
        private void ShowUnitStatus(Unit unit)
        {
            presentation.DisplayMessage($"Date {GameDate} ({unit.x},{unit.y}) {unit.alignment.ToString()} {unit.ToString()}");
        }

        //---------------------------------------------------------------------
        // UpdateEnemyMap
        // ==============
        //
        // Update the enemy's map around the specified location
        //---------------------------------------------------------------------
        public void UpdateEnemyMap(int xCentre, int yCentre)
        {
            UpdateMap(enemyMap, xCentre, yCentre);
        }

        //---------------------------------------------------------------------
        // UpdateEnemyMap
        // ==============
        //
        // Update the enemy's map around the specified location
        //---------------------------------------------------------------------
        public void UpdateEnemyMap(Unit unit)
        {
            UpdateMap(enemyMap, unit.x, unit.y);
        }

        //---------------------------------------------------------------------
        // UpdateMap
        // =========
        //
        // Update the specified map around the specified location
        //---------------------------------------------------------------------
        private void UpdateMap(Map mapUpdate, int xCentre, int yCentre)
        {
            int x;
            int y;

            for (int dy = -1; dy <= 1; dy++)
            {
                y = yCentre + dy;

                for (int dx = -1; dx <= 1; dx++)
                {
                    x = xCentre + dx;

                    // If we're not off the map...
                    if (mapUpdate.EdgeCheck(x, y, 0, 0))
                    {
                        // What is on the master map
                        Terrain location = map.Location(x, y);

                        mapUpdate.Location(x, y).type = location.type;
                        mapUpdate.Location(x, y).occupant = location.occupant;
                        mapUpdate.Location(x, y).city = location.city;
                    }
                }
            }
        }

        //---------------------------------------------------------------------
        // UpdatePlayerMap
        // ===============
        //
        // Update the players's map around the specified location
        //---------------------------------------------------------------------
        public void UpdatePlayerMap(int xCentre, int yCentre)
        {
            UpdateMap(playerMap, xCentre, yCentre);
        }

        //---------------------------------------------------------------------
        // UpdatePlayerMap
        // ===============
        //
        // Update the players's map around the specified unit
        //---------------------------------------------------------------------
        public void UpdatePlayerMap(Unit unit)
        {
            UpdateMap(playerMap, unit.x, unit.y);
        }

        //---------------------------------------------------------------------
        // Enemy unit temporary handling
        //---------------------------------------------------------------------
        //---------------------------------------------------------------------
        // ChooseEnemyCityProduction
        // =========================
        //
        // Ask the player to choose production for a city
        //---------------------------------------------------------------------
        public void ChooseEnemyCityProduction(City city)
        {
            UnitType production = presentation.ChooseCityProduction(enemyMap, city);
            city.BeginProduction(production, GameDate);
        }

        private void MoveEnemyUnit(Army army)
        {
            Terrain location = map.Location(army.x, army.y);

            // Don't ask for orders if either of the following is true...
            //  •   the army is in sentry mode
            //  •   the army is at sea, and there is no adjecent terrain
            //  •   the army is embarked on a helo, and the helo is overwater
            //      (this also works when the helo is itself embrked
            //
            if (
                    !army.Sentry
                && !(army.Embarked && enemyMap.SeaFrontage(army.x, army.y) == 8)
                && !(army.EmbarkedOn is Helicopter && location.type == TerrainType.Sea)
                )
            {
                ActionType actionType;
                int dx = 0;
                int dy = 0;
                int xDestination;
                int yDestination;

                UpdateEnemyMap(army.x, army.y);

                do
                {
                    actionType = ActionType.Invalid;
                    OrderType orders = presentation.GetOrders(army.x, army.y);

                    // Orders are to stay at current location
                    // We can do this if the army is embarked, if the current location is not a city, or if
                    // the army is out in the open
                    if (orders == OrderType.None)
                    {
                        // Can't remain in a city
                        if (location.type == TerrainType.City)
                        {
                            actionType = ActionType.Invalid;
                        }
                    }

                    // Orders were to enable sentry mode
                    // This can be done of the army is not embarked, and the army is not in a city
                    else if (orders == OrderType.Sentry && !(army.Embarked || location.type == TerrainType.City))
                    {
                        actionType = ActionType.None;
                        army.Sentry = true;
                    }

                    // Otherwise orders are to move
                    // If movement puts army on map edge then Invalid action type is returned
                    GetDeltas(orders, out dx, out dy);
                    actionType = GetEnemyAction(army, dx, dy);

                    if (actionType == ActionType.Invalid)
                    {
                        presentation.Alert();
                    }
                } while (actionType == ActionType.Invalid);

                // Orders are valid
                // Make sure the result of the orders is valid
                xDestination = army.x + dx;
                yDestination = army.y + dy;

                Terrain destination = map.Location(xDestination, yDestination);

                // What action is being taken?
                switch (actionType)
                {
                    case ActionType.AttackUnit:
                        // If the army is attacking from a transport,get off
                        if (army.Embarked)
                        {
                            army.Disembark();
                        }

                        // The army vacates the original location
                        else if (location.occupant == army)
                        {
                            location.occupant = null;
                        }

                        // Winner takes the destination
                        // Then update the maps around the two locations
                        destination.occupant = Combat(army, destination.occupant, xDestination, yDestination);
                        UpdatePlayerMap(army.x, army.y);
                        UpdatePlayerMap(xDestination, yDestination);
                        UpdateEnemyMap(army.x, army.y);
                        UpdateEnemyMap(xDestination, yDestination);
                        break;

                    case ActionType.EmbarkTransport:
                    case ActionType.EmbarkHelicopter:
                        {
                            //
                            // Get the helicopter or troop transporter
                            // Add the army to its occupants
                            // End the army's turn by removing remaining movement
                            // Remove the army from its last location
                            //
                            ITransporter transporter = (ITransporter)map.Location(xDestination, yDestination).occupant;

                            if (!transporter.IsFull)
                            {
                                army.Embark(transporter);
                            }

                            location.occupant = null;
                        }
                        break;

                    // Simple move
                    case ActionType.Move:
                        {
                            destination.occupant = army;

                            // If moving out of a city, remove from the list of occupants
                            if (location.type == TerrainType.City)
                            {
                                location.city.occupants.Remove(army);
                            }

                            // If disembarking, remove from the list of occupants
                            else if (army.Embarked)
                            {
                                army.Disembark();
                            }

                            // If this was a simple move
                            if (location.occupant == army)
                            {
                                location.occupant = null;
                            }
                        }
                        break;

                    // Stay where we are.
                    case ActionType.None:
                        break;

                    case ActionType.AttackCity:
                        {
                            // Attack the city
                            City city = destination.city;
                            Alignment alignment = city.alignment;

                            AttackCity(army, city);

                            // Whatever the outcome the army is removed from the field
                            army.Destroyed = true;
                            if (location.occupant == army)
                            {
                                location.occupant = null;
                            }
                            else
                            {
                                army.Disembark();
                            }

                            switch (alignment)
                            {
                                case Alignment.Player:
                                    UpdatePlayerMap(city.x, city.y);
                                    break;
                                case Alignment.Enemy:
                                    UpdateEnemyMap(city.x, city.y);
                                    break;
                            }
                        }
                        break;

                    case ActionType.ArmyTransfer:
                        {
                            // Remove the army from the transport at the current location
                            // Add the army to the transport at the destination
                            army.Disembark();
                            army.Embark((ITransporter)destination.occupant);
                        }
                        break;

                    case ActionType.Invalid:
                        presentation.Alert();
                        break;
                }

                // We have emerged with a valid action
                // Update army's location
                // Update master and player maps
                army.x = xDestination;
                army.y = yDestination;
                UpdateMap(enemyMap, xDestination, yDestination);
                presentation.UpdateAdjacent(enemyMap, xDestination, yDestination);
            }
        }

        private void MoveEnemyUnit(Fighter fighter)
        {
            Terrain location = map.Location(fighter.x, fighter.y);
            Terrain destination;
            ActionType actionType;

            int dx = 0;
            int dy = 0;
            int xDestination;
            int yDestination;
            int movement = Math.Min(fighter.Movement, fighter.Range);

            // If the fighter exhausted its fuel last turn, mark it as destroyed
            if (movement <= 0)
            {
                fighter.Destroyed = true;
            }

            while (movement > 0)
            {
                do
                {
                    // Get some orders
                    OrderType orders = presentation.GetOrders(fighter.x, fighter.y);

                    // Orders are to stay at current location
                    // We can do this if the fighter is embarked, or if the current location is not a city
                    if (orders == OrderType.None && (fighter.Embarked || location.type != TerrainType.City))
                    {
                        actionType = ActionType.None;
                    }

                    // Orders were to enable sentry mode - Not permitted for fighters
                    else if (orders == OrderType.Sentry)
                    {
                        actionType = ActionType.Invalid;
                    }

                    // Otherwise orders are to move - if the movement would put the fighter
                    // on the edge of the map, then the Invalid action type is returned
                    else
                    {
                        GetDeltas(orders, out dx, out dy);
                        actionType = GetEnemyAction(fighter, dx, dy);
                    }

                    if (actionType == ActionType.Invalid)
                    {
                        presentation.Alert();
                    }
                } while (actionType == ActionType.Invalid);

                // Orders are valid
                // Make sure the result of the orders is valid
                xDestination = fighter.x + dx;
                yDestination = fighter.y + dy;
                destination = map.Location(xDestination, yDestination);

                // What action is being taken?
                switch (actionType)
                {
                    case ActionType.AttackUnit:
                        Combat(fighter, destination.occupant, xDestination, yDestination);
                        break;

                    // Simple move
                    case ActionType.Move:
                        {
                            // If moving out of a city, remove from the list of occupants
                            if (location.type == TerrainType.City)
                            {
                                location.city.occupants.Remove(fighter);
                            }

                            // If disembarking a carrier, remove from the list of occupants
                            else if (fighter.Embarked)
                            {
                                ((AircraftCarrier)location.occupant).Passengers.Remove(fighter);
                                fighter.Disembark();
                            }

                            // If this was a simple move
                            if (location.occupant == fighter)
                            {
                                location.occupant = null;
                            }

                            // Put the fighter on the map at the new location
                            // Reduce avaiable range
                            // Reduce remaining movement
                            // Set source location to destination now that we've moved
                            destination.occupant = fighter;
                            fighter.ConsumeFuel();
                            movement--;
                            location = destination;
                        }
                        break;

                    // Land fighter in city
                    case ActionType.LandAirbase:
                        {
                            //
                            // Add the fighter to the city's occupants
                            // Refuel the fighter
                            // Remove the fighter from its last location
                            // End the fighter's turn by removing remaining movement
                            //
                            destination.city.occupants.Add(fighter);
                            fighter.Refuel();
                            location.occupant = null;
                            movement = 0;
                        }
                        break;

                    case ActionType.LandVessel:
                        {
                            //
                            // Add the aircraft to the carrier's occupants
                            // Refuel the aircraft
                            // Remove the aircraft from its last location
                            // End the aircraft's turn by removing remaining movement
                            //
                            fighter.Embark((AircraftCarrier)destination.occupant);
                            fighter.Refuel();
                            location.occupant = null;
                            movement = 0;
                        }
                        break;

                    case ActionType.AircraftTransfer:
                        {
                            // Remove the fighter from the carrier at the current location
                            // Add the fighter to the carrier at the destination
                            // Refuel the fighter
                            // End movement
                            fighter.Disembark();
                            fighter.Embark((AircraftCarrier)destination.occupant);
                            fighter.Refuel();
                            movement = 0;
                        }
                        break;

                    // Stay where we are.
                    case ActionType.None:
                        // Consume fuel for remaining movement
                        // End movement
                        if (!fighter.Embarked)
                        {
                            fighter.ConsumeFuel(movement);
                        }

                        movement = 0;
                        break;
                }

                // We have performed an action
                // Update fighters's location
                // Update master and player maps
                fighter.x = xDestination;
                fighter.y = yDestination;
                UpdateMap(enemyMap, xDestination, yDestination);
                presentation.UpdateAdjacent(enemyMap, xDestination, yDestination);
            }
        }

        private void MoveEnemyUnit(Helicopter helicopter)
        {
            Terrain location = map.Location(helicopter.x, helicopter.y);
            ActionType actionType;

            int dx = 0;
            int dy = 0;
            int xDestination;
            int yDestination;
            int movement = Math.Min(helicopter.Movement, helicopter.Range);

            // If we're waiting for passengers, don't bother asking for orders
            if (helicopter.Fill)
            {
                movement = 0;
            }

            // If the helicopter exhausted its fuel last turn, mark it as destroyed
            if (movement <= 0)
            {
                helicopter.Destroyed = true;
            }

            while (movement > 0)
            {
                do
                {
                    OrderType orders = presentation.GetOrders(helicopter.x, helicopter.y);

                    // Orders are to stay at current location
                    // We can do this if the helicopter is embarked, or if the current location is not a city
                    if (orders == OrderType.None && (helicopter.Embarked || location.type != TerrainType.City))
                    {
                        actionType = ActionType.None;
                    }

                    // Orders were to enable sentry mode - Not permitted for helicopters
                    else if (orders == OrderType.Sentry)
                    {
                        actionType = ActionType.Invalid;
                    }

                    // Sleep until full... can do this only if we're on the ground
                    else if (orders == OrderType.Fill)
                    {
                        if (!helicopter.Embarked && location.occupant == helicopter && location.type == TerrainType.Land)
                        {
                            actionType = ActionType.Fill;
                        }
                        else
                        {
                            actionType = ActionType.Invalid;
                        }
                    }

                    // Otherwise orders are to move.
                    // If move would put helicopter on the edge of the map, then invalid action type returned
                    else
                    {
                        GetDeltas(orders, out dx, out dy);
                        actionType = GetEnemyAction(helicopter, dx, dy);
                    }

                    if (actionType == ActionType.Invalid)
                    {
                        presentation.Alert();
                    }
                } while (actionType == ActionType.Invalid);

                // Orders are valid
                // Make sure the result of the orders is valid
                xDestination = helicopter.x + dx;
                yDestination = helicopter.y + dy;

                Terrain destination = map.Location(xDestination, yDestination);

                // What action is being taken?
                switch (actionType)
                {
                    case ActionType.AttackUnit:
                        Combat(helicopter, destination.occupant, xDestination, yDestination);
                        break;

                    // Simple move
                    case ActionType.Move:
                        {
                            // If moving out of a city, remove from the list of occupants
                            if (location.type == TerrainType.City)
                            {
                                location.city.occupants.Remove(helicopter);
                            }

                            // If disembarking an assualt ship, remove from the list of occupants
                            else if (helicopter.Embarked)
                            {
                                helicopter.Disembark();
                            }

                            // If this was a simple move
                            if (location.occupant == helicopter)
                            {
                                location.occupant = null;
                            }

                            // Put the helicopter on the map at the new location
                            // Bring passengers
                            // Reduce avaiable range
                            // Reduce remaining movement
                            // Change source location now that we've moved
                            destination.occupant = helicopter;
                            helicopter.ConsumeFuel();
                            movement--;
                            location = destination;
                        }
                        break;

                    // Land helicopter in city
                    case ActionType.LandAirbase:
                        {
                            //
                            // Add the helicopter to the city's occupants
                            // Bring passengers
                            // Refuel the helicopter
                            // Remove the helicopter from its last location
                            // End the helicopter's turn by removing remaining movement
                            //
                            destination.city.occupants.Add(helicopter);
                            helicopter.Refuel();
                            location.occupant = null;
                            movement = 0;
                        }
                        break;

                    case ActionType.LandVessel:
                        {
                            //
                            // Add the helicopter to the assault ship's occupants
                            // Refuel the helicopter
                            // Remove the helicopter from its last location
                            // End the helicopter's turn by removing remaining movement
                            //
                            helicopter.Embark((AssaultShip)destination.occupant);
                            helicopter.Refuel();
                            location.occupant = null;
                            movement = 0;
                        }
                        break;

                    case ActionType.AircraftTransfer:
                        {
                            // Remove the helicopter from the carrier at the current location
                            // Add the helicopter to the carrier at the destination
                            // Change the vessel that the helicopter is embarked on
                            // Bring passengers
                            // Refuel the helicopter
                            // End movement
                            helicopter.Disembark();
                            helicopter.Embark((AssaultShip)destination.occupant);
                            helicopter.Refuel();
                            movement = 0;
                        }
                        break;

                    case ActionType.Fill:
                        if (!helicopter.Embarked && location.occupant == helicopter && location.type == TerrainType.Land)
                        {
                            helicopter.Fill = true;
                            movement = 0;
                        }
                        break;

                    // Stay where we are.
                    case ActionType.None:
                        // Consume fuel for remaining movement
                        // End movement
                        if (!helicopter.Embarked)
                        {
                            helicopter.ConsumeFuel(movement);
                        }
                        movement = 0;
                        break;
                }

                // We have performed an action
                // Update helicopters's location
                // Update master and player maps
                helicopter.x = xDestination;
                helicopter.y = yDestination;
                helicopter.ConveyPassengers();
                UpdateMap(enemyMap, xDestination, yDestination);
                presentation.UpdateAdjacent(enemyMap, xDestination, yDestination);
            }
        }

        private void MoveEnemyUnit(Unit unit)
        {
            Terrain location = map.Location(unit.x, unit.y);
            int movement = unit.Movement;

            // Don't ask for orders if the unit is in sentry mode
            // Don't ask for orders if a transport is in fill mode
            if ((unit is ISentry && ((ISentry)unit).Sentry) || (unit is ITransporter && ((ITransporter)unit).Fill))
            {
                movement = 0;
            }

            ActionType actionType;
            int dx;
            int dy;
            int xDestination;
            int yDestination;

            while (movement > 0)
            {
                do
                {
                    dx = 0;
                    dy = 0;

                    OrderType orders = presentation.GetOrders(unit.x, unit.y);

                    // Orders are to stay at current location
                    if (orders == OrderType.None)
                    {
                        actionType = ActionType.None;
                    }

                    // Orders were to enable sentry mode
                    // This can be done if the unit is not in a city
                    else if (orders == OrderType.Sentry && location.type != TerrainType.City)
                    {
                        actionType = ActionType.None;
                        ((ISentry)unit).Sentry = true;
                    }

                    // Sleep until full... can do this only if we're at sea
                    else if (orders == OrderType.Fill)
                    {
                        if (location.type == TerrainType.Sea)
                        {
                            actionType = ActionType.Fill;
                        }
                        else
                        {
                            actionType = ActionType.Invalid;
                        }
                    }

                    // Otherwise orders are to move.
                    // If movement would put unit on map edge, invalid action type is returned
                    else
                    {
                        GetDeltas(orders, out dx, out dy);
                        actionType = GetEnemyAction(unit, dx, dy);
                    }

                    if (actionType == ActionType.Invalid)
                    {
                        presentation.Alert();
                    }
                } while (actionType == ActionType.Invalid);

                // Orders are valid
                // Make sure the result of the orders is valid
                xDestination = unit.x + dx;
                yDestination = unit.y + dy;

                Terrain destination = map.Location(xDestination, yDestination);

                // What action is being taken?
                switch (actionType)
                {
                    case ActionType.AttackUnit:
                        Combat(unit, destination.occupant, xDestination, yDestination);
                        break;

                    // Simple move
                    case ActionType.Move:
                        {
                            // If moving out of a city, remove from the list of occupants
                            if (location.type == TerrainType.City)
                            {
                                location.city.occupants.Remove(unit);
                            }

                            // If this was a simple move
                            else if (location.occupant == unit)
                            {
                                location.occupant = null;
                            }

                            // Reduce available movement
                            // Change source location now that we've moved
                            movement--;
                            location = destination;
                            destination.occupant = unit;
                        }
                        break;

                    // Enter friendly city
                    case ActionType.Dock:
                        {
                            //
                            // Add the vessel to the city's occupants
                            // End the vessel's turn by removing remaining movement
                            // Remove the vessel from its last location
                            //
                            destination.city.occupants.Add(unit);
                            location.occupant = null;
                            movement = 0;
                        }
                        break;

                    // Sleep until full
                    case ActionType.Fill:
                        if (unit is ITransporter transporter)
                        {
                            transporter.Fill = true;
                            movement = 0;
                        }
                        break;

                    // Stay where we are.
                    case ActionType.None:
                        movement = 0;
                        break;

                    case ActionType.Invalid:
                        presentation.Alert();
                        break;
                }

                // We have emerged with a valid action
                // Update unit's location
                // Bring passengers
                // Update master and player maps
                unit.x = xDestination;
                unit.y = yDestination;

                if (unit is ITransporter)
                {
                    ((ITransporter)unit).ConveyPassengers();
                }

                UpdateMap(enemyMap, xDestination, yDestination);
                presentation.UpdateAdjacent(enemyMap, xDestination, yDestination);
            }
        }

        //---------------------------------------------------------------------
        // GetEnemyAction
        // ==============
        //
        // Identifies the action being performed
        //---------------------------------------------------------------------
        private ActionType GetEnemyAction(Unit unit, int dx, int dy)
        {
            ActionType actionType;

            if (!map.EdgeCheck(unit.x, unit.y, dx, dy))
            {
                actionType = ActionType.Invalid;
            }
            else
            {
                Terrain destination = map.Location(unit.x + dx, unit.y + dy);

                switch (unit.type)
                {
                    case UnitType.Army:
                        actionType = GetEnemyUnitAction((Army)unit, destination);
                        break;

                    case UnitType.Fighter:
                        actionType = GetEnemyUnitAction((Fighter)unit, destination);
                        break;

                    case UnitType.Helicopter:
                        actionType = GetEnemyUnitAction((Helicopter)unit, destination);
                        break;

                    default:
                        actionType = GetEnemyUnitAction(unit, destination);
                        break;
                }
            }

            return actionType;
        }

        //---------------------------------------------------------------------
        // GetEnemyUnitAction
        // ==================
        //
        // Identifies the action being performed for army units
        //---------------------------------------------------------------------
        private ActionType GetEnemyUnitAction(Army army, Terrain destination)
        {
            ActionType actionType = ActionType.Invalid;

            // If underlying terrain at the destination is sea
            if (destination.type == TerrainType.Sea)
            {
                // Can't move into open water
                if (destination.occupant == null)
                    actionType = ActionType.Invalid;

                // If we're on a troop transport or helicopter
                else if (army.Embarked)
                {
                    // We're on a troop transport
                    if (((Unit)army.EmbarkedOn).type == UnitType.TroopTransport)
                    {
                        // If we're already on this transport, then we're staying onboard
                        if (destination.occupant == army.EmbarkedOn)
                            actionType = ActionType.None;

                        // If the occupant of the destination is an enemy, attack it
                        else if (destination.occupant.alignment == Alignment.Player)
                            actionType = ActionType.AttackUnit;

                        // If it is a friendly troop transport, can we embark?
                        else if (destination.occupant.type == UnitType.TroopTransport)
                        {
                            // If there's space on the transport...
                            if (!((ITransporter)destination.occupant).IsFull)
                                actionType = ActionType.ArmyTransfer;
                            else
                                actionType = ActionType.Invalid;
                        }

                        // Don't allow attacking own vessels
                        else
                            actionType = ActionType.Invalid;
                    }

                    // We're in a helicopter, we can't attack or board another ship
                    else
                    {
                        // If we're already on this helicopter, then we're staying onboard
                        if (destination.occupant == army.EmbarkedOn)
                            actionType = ActionType.None;

                        // If we're on a helicopter that is aboard an assualt ship.
                        else if (((Helicopter)army.EmbarkedOn).EmbarkedOn == destination.occupant)
                            actionType = ActionType.None;

                        else
                            actionType = ActionType.Invalid;
                    }
                }

                // We're not embarked on anything
                else
                {
                    // If the occupant of the destination is an enemy, attack it
                    if (destination.occupant.alignment == Alignment.Player)
                        actionType = ActionType.AttackUnit;

                    // If it is a friendly troop transport, can we embark?
                    else if (destination.occupant.type == UnitType.TroopTransport)
                    {
                        // If there's space on the transport...
                        if (!((ITransporter)destination.occupant).IsFull)
                            actionType = ActionType.EmbarkTransport;
                        else
                            actionType = ActionType.Invalid;
                    }

                    // Don't allow attacking own units
                    else
                        actionType = ActionType.Invalid;
                }
            }

            // If destination is another city
            else if (destination.type == TerrainType.City)
            {
                // If we're in a transport, and that transport is docked, or landed, in the city
                // then we can remain on board the transport
                if (army.Embarked && destination.city.occupants.Contains((Unit)army.EmbarkedOn))
                    actionType = ActionType.None;

                // Can't enter our cities
                else if (destination.city.alignment == Alignment.Enemy)
                    actionType = ActionType.Invalid;

                // But other cities can be attacked
                else
                    actionType = ActionType.AttackCity;
            }

            // If destination is land...
            else if (destination.type == TerrainType.Land)
            {
                // If land is empty, just move
                if (destination.occupant == null)
                    actionType = ActionType.Move;

                // If it is this army standing still 
                // out in the open 
                // on board a helicopter
                // on board a helicopter that is on board an assault ship
                else if (
                            destination.occupant == army ||
                            army.EmbarkedOn == destination.occupant ||
                            (army.EmbarkedOn is Helicopter helicopter && helicopter.EmbarkedOn == destination.occupant)
                        )
                    actionType = ActionType.None;

                else
                {
                    // Friendly forces...
                    if (destination.occupant.alignment == Alignment.Enemy)
                    {
                        // It's a helicopter with room to spare
                        if (destination.occupant.type == UnitType.Helicopter && !((ITransporter)destination.occupant).IsFull)
                        {
                            actionType = ActionType.EmbarkHelicopter;
                        }

                        // Don't attack friendly forces
                        else
                            actionType = ActionType.Invalid;
                    }

                    // Otherwise attack enemy forces
                    else
                        actionType = ActionType.AttackUnit;
                }
            }

            return actionType;
        }

        //---------------------------------------------------------------------
        // GetEnemyUnitAction
        // ==================
        //
        // Identifies the action being performed for fighter units
        //---------------------------------------------------------------------
        private ActionType GetEnemyUnitAction(Fighter fighter, Terrain destination)
        {
            ActionType actionType;

            // If destination is sea
            if (destination.type == TerrainType.Sea)
            {
                // Overwater flight
                if (destination.occupant == null)
                    actionType = ActionType.Move;
                else
                {
                    // If the occupant is an enemy vessel, attack it
                    if (destination.occupant.alignment == Alignment.Player)
                        actionType = ActionType.AttackUnit;

                    // If it is a friendly aircraft carrier, land on board
                    else if (destination.occupant.type == UnitType.AircraftCarrier && !((AircraftCarrier)destination.occupant).IsFull)
                        // If we're already on a carrier, perform a transfer
                        if (fighter.Embarked)
                            actionType = ActionType.AircraftTransfer;
                        else
                            actionType = ActionType.LandVessel;

                    // Don't allow attacking own vessels
                    else
                        actionType = ActionType.Invalid;
                }
            }

            // If destination is a city
            else if (destination.type == TerrainType.City)
            {
                // If it is one of our cities, land there
                if (destination.city.alignment == Alignment.Enemy)
                    actionType = ActionType.LandAirbase;

                // Otherwise, don't enter city
                else
                    actionType = ActionType.Invalid;
            }

            // Destination is land
            else
            {
                // If empty land, continue flight
                if (destination.occupant == null)
                    actionType = ActionType.Move;

                // If the land is not empty
                else
                {
                    // Don't attack our own forces
                    if (destination.occupant.alignment == Alignment.Enemy)
                        actionType = ActionType.Invalid;

                    // Attack enemy units
                    else
                        actionType = ActionType.AttackUnit;
                }
            }

            return actionType;
        }

        //---------------------------------------------------------------------
        // GetEnemyUnitAction
        // ==================
        //
        // Identifies the action being performed for helicopter units
        //---------------------------------------------------------------------
        private ActionType GetEnemyUnitAction(Helicopter helicopter, Terrain destination)
        {
            ActionType actionType;

            // If destination is sea
            if (destination.type == TerrainType.Sea)
            {
                // If we're loitering
                if (destination.occupant == helicopter || (helicopter.Embarked && destination.occupant == helicopter.EmbarkedOn))
                    actionType = ActionType.None;

                // Overwater flight
                else if (destination.occupant == null)
                    actionType = ActionType.Move;

                else
                {
                    // If the occupant is an enemy vessel, attack it
                    if (destination.occupant.alignment == Alignment.Player)
                        actionType = ActionType.AttackUnit;

                    // If it is a friendly assault ship, land on board
                    else if (destination.occupant.type == UnitType.AssaultShip && !((AssaultShip)destination.occupant).IsFull)
                        // If we're already embarked, perform a transfer
                        if (helicopter.Embarked)
                            actionType = ActionType.AircraftTransfer;
                        else
                            actionType = ActionType.LandVessel;

                    // Don't allow attacking own vessels
                    else
                        actionType = ActionType.Invalid;
                }
            }

            // If destination is a city
            else if (destination.type == TerrainType.City)
            {
                // If it is one of our cities, land there
                if (destination.city.alignment == Alignment.Enemy)
                    actionType = ActionType.LandAirbase;

                // Otherwise, don't enter city
                else
                    actionType = ActionType.Invalid;
            }

            // Destination is land
            else
            {
                // If empty land, continue flight
                if (destination.occupant == null)
                    actionType = ActionType.Move;

                // If the land is not empty
                else
                {
                    // If the terrain occupant is this piece, then we're loitering
                    if (destination.occupant == helicopter)
                        actionType = ActionType.None;

                    // Don't attack our own forces
                    else if (destination.occupant.alignment == Alignment.Enemy)
                        actionType = ActionType.Invalid;

                    // Attack enemy units
                    else
                        actionType = ActionType.AttackUnit;
                }
            }

            return actionType;
        }

        //---------------------------------------------------------------------
        // GetEnemyUnitAction
        // ==================
        //
        // Identifies the action being performed for other units
        //---------------------------------------------------------------------
        private ActionType GetEnemyUnitAction(Unit unit, Terrain destination)
        {
            ActionType actionType;

            // If destination is sea
            if (destination.type == TerrainType.Sea)
            {
                // Continue sailing
                if (destination.occupant == null)
                    actionType = ActionType.Move;

                // If we're staying put.
                else if (destination.occupant == unit)
                    actionType = ActionType.None;

                else
                {
                    // If the occupant is an enemy vessel, attack it
                    if (destination.occupant.alignment == Alignment.Player)
                        actionType = ActionType.AttackUnit;

                    // Don't allow attacking own vessels
                    else
                        actionType = ActionType.Invalid;
                }
            }

            // If destination is a city
            else if (destination.type == TerrainType.City)
            {
                // If it is one of our cities, dock there
                if (destination.city.alignment == Alignment.Enemy)
                    actionType = ActionType.Dock;

                // Otherwise, don't attack city
                else
                    actionType = ActionType.Invalid;
            }
            // Destination is land
            else
                actionType = ActionType.Invalid;

            return actionType;
        }
    }
}
