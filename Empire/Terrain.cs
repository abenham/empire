using System.Text;

namespace Empire
{
    public enum TerrainType
    {
        None,
        Sea,
        Land,
        City
    };

    class Terrain
    {

        public bool seenByPlayer;
        public bool seenByComputer;
        public TerrainType type;
        public int body;
        public Unit occupant;
        public City city;

        public Terrain()
        {
            this.occupant = null;
            this.city = null;
            this.type = TerrainType.Sea;
            this.seenByPlayer = false;
            this.seenByComputer = false;
            this.body = -1;
        }

        public Terrain(TerrainType terrainType)
        {
            this.occupant = null;
            this.city = null;
            this.type = terrainType;
            this.seenByPlayer = false;
            this.seenByComputer = false;
            this.body = -1;
        }
        public string Help()
        {
            StringBuilder bob = new StringBuilder();

            bob.Append("TerrainType = ");
            bob.AppendLine(this.HelpTerrainType(this.type));
            bob.Append("Seen by player = ");
            bob.AppendLine(this.seenByPlayer.ToString());
            bob.Append("Seen by computer = ");
            bob.AppendLine(this.seenByComputer.ToString());

            return bob.ToString();
        }

        public override string ToString()
        {
            string symbol = " ";

            if (occupant != null)
            {
                switch (occupant.type)
                {
                    case UnitType.Army:
                        symbol = "A";
                        break;

                    case UnitType.Fighter:
                        symbol = "F";
                        break;

                    case UnitType.Helicopter:
                        symbol = "H";
                        break;

                    case UnitType.Submarine:
                        symbol = "S";
                        break;

                    case UnitType.Destroyer:
                        symbol = "D";
                        break;

                    case UnitType.Cruiser:
                        symbol = "R";
                        break;

                    case UnitType.Battleship:
                        symbol = "B";
                        break;

                    case UnitType.AircraftCarrier:
                        symbol = "C";
                        break;

                    case UnitType.AssaultShip:
                        symbol = "L";
                        break;

                    case UnitType.TroopTransport:
                        symbol = "T";
                        break;
                }

                if (occupant.alignment == Alignment.Enemy)
                    symbol = symbol.ToLower();
            }
            else
            {
                switch (this.type)
                {
                    case TerrainType.Sea:
                        symbol = ".";
                        break;

                    case TerrainType.Land:
                        symbol = "+";
                        break;

                    case TerrainType.City:
                        if (this.city != null)
                            switch (this.city.alignment)
                            {
                                case Alignment.Enemy:
                                    symbol = "X";
                                    break;
                                case Alignment.Player:
                                    symbol = "O";
                                    break;
                                default:
                                    symbol = "*";
                                    break;
                            }
                        else
                            symbol = "?";
                        break;
                }
            }

            return symbol;
        }

        public string HelpTerrainType(TerrainType type)
        {
            string description;
            switch (type)
            {
                case TerrainType.Sea:
                    description = "Sea";
                    break;

                case TerrainType.Land:
                    description = "Land";
                    break;

                case TerrainType.City:
                    description = "City";
                    break;

                default:
                    description = "Unknown";
                    break;
            }

            return description;
        }
    }
}
