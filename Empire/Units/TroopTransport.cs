using System.Collections.ObjectModel;

namespace Empire
{
    class TroopTransport : Unit, ITransporter, ISentry
    {
        public TroopTransport(int id)
        {
            Id = id;
            type = UnitType.TroopTransport;
            hits = 3;
            strength = 3;
            damage = 1;
            Movement = 2;
            Passengers = new Collection<Unit>();
        }

        public override string ToString()
        {
            return $"troop transport #{Id} hits {hits} movement {Movement} passengers {Passengers.Count} capacity {Capacity} sentry {Sentry}";
        }

        public new bool Destroyed
        {
            get
            {
                return base.Destroyed;
            }
            set
            {
                foreach (Unit passenger in Passengers)
                {
                    passenger.Destroyed = true;
                }
            }
        }

        //======================================================================
        // ITransporter
        //======================================================================
        public Collection<Unit> Passengers { get; set; }
        public int Capacity { get { return hits * 2; } }
        public bool Fill { get; set; }
        public bool IsFull
        {
            get
            {
                return (Passengers.Count == Capacity);
            }
        }
        public void ConveyPassengers()
        {
            foreach (Unit passenger in Passengers)
            {
                passenger.x = x;
                passenger.y = y;
            }
        }

        public void AddPassenger(Unit passenger)
        {
            Passengers.Add(passenger);
            if (Passengers.Count == Capacity && Fill)
            {
                Fill = false;
            }
        }

        //======================================================================
        // ISentry
        //======================================================================
        public bool Sentry { get; set; }

        public bool ToggleSentry()
        {
            Sentry = !Sentry;
            return Sentry;
        }
    }
}
