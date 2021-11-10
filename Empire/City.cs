using System.Collections.ObjectModel;

namespace Empire
{
    class City
    {
        public Collection<Unit> occupants;
        public int x;
        public int y;
        public bool landLocked;
        public Alignment alignment;
        public int hits;
        private readonly int strength;
        public int damage;
        private const int STRENGTH = 3;

        public City(int x, int y)
        {
            alignment = Alignment.Neutral;
            occupants = new Collection<Unit>();
            this.x = x;
            this.y = y;
            hits = STRENGTH;
            strength = STRENGTH;
            damage = 1;
        }

        public void BeginProduction(UnitType type, int gameDate)
        {
            Production = type;
            Completion = gameDate + Unit.GetConstructionTime(type);
        }

        public int Completion { get; private set; }
        
        public UnitType Production { get; private set; }

        public bool ProductionComplete(int gameDate)
        {
            bool complete = false;

            if (gameDate >= Completion)
            {
                complete = true;
                Completion = gameDate + Unit.GetConstructionTime(Production);
            }

            return complete;
        }
        
        public override string ToString()
        {
            int ships = 0;
            int aircraft = 0;

            foreach (Unit unit in occupants)
            {
                if (unit is IAircraft)
                    aircraft++;
                else if (!(unit is Army))
                    ships++;
            }

            string text;

            if (alignment == Alignment.Player)
            {
                text = $"Your city hits {hits}, producing {Production.ToString()}, completion {Completion}, aircraft {aircraft}, ships {ships}";
            }
            else
            {
                text = $"{alignment.ToString()} city hits {hits}";
            }

            return text;
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
