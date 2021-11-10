using System.Diagnostics;

namespace Empire
{
    public enum UnitType
    {
        None,
        Army,
        Fighter,
        Submarine,
        Destroyer,
        Cruiser,
        Battleship,
        AircraftCarrier,
        TroopTransport,
        Helicopter,
        AssaultShip
    }

    class Unit
    {
        public int Id { get; protected set; }
        public UnitType type;
        public int hits;
        public int strength;
        public int damage;
        public int x;
        public int y;
        public bool Destroyed { get; set; }
        public Alignment alignment;

        public int Movement { get; protected set; }

        public Unit()
        {
            this.Movement = 1;
            this.damage = 1;
            this.Destroyed = false;
            this.hits = 0;
            this.strength = 0;
        }

        ~Unit()
        {
            Debug.WriteLine("Unit is being destroyed");
            Debug.WriteLine("id {0} type {1}", Id, type.ToString());
            Debug.WriteLine("Location ({0},{1})", x, y);
            Debug.WriteLine("Destroyed {0}", Destroyed);
        }

        public static int GetConstructionTime(UnitType type)
        {
            int constructionTime = 0;

            switch (type)
            {
                case UnitType.None:
                    constructionTime = 0;
                    break;
                case UnitType.Army:
                    constructionTime = 6;
                    break;
                case UnitType.Fighter:
                    constructionTime = 9;
                    break;
                case UnitType.Submarine:
                    constructionTime = 30;
                    break;
                case UnitType.Destroyer:
                    constructionTime = 24;
                    break;
                case UnitType.Cruiser:
                    constructionTime = 60;
                    break;
                case UnitType.Battleship:
                    constructionTime = 80;
                    break;
                case UnitType.AircraftCarrier:
                    constructionTime = 72;
                    break;
                case UnitType.AssaultShip:
                    constructionTime = 72;
                    break;
                case UnitType.TroopTransport:
                    constructionTime = 36;
                    break;
                case UnitType.Helicopter:
                    constructionTime = 9;
                    break;
                default:
                    break;
            }

            return constructionTime;
        }

        public void Repair()
        {
            if (hits < strength)
            {
                hits++;
            }
        }
    }
}
