using System.Collections.ObjectModel;

namespace Empire
{
    class AssaultShip : Unit, ITransporter, ISentry
    {
        public AssaultShip(int id)
        {
            Id = id;
            type = UnitType.AssaultShip;
            hits = 8;
            strength = 8;
            damage = 1;
            Movement = 2;
            Passengers = new Collection<Unit>();
        }

        public override string ToString()
        {
            return $"assault ship #{Id} hits {hits} movement {Movement} passengers {Passengers.Count} capacity {Capacity} sentry {Sentry}";
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

        public void AddPassenger(Unit passenger)
        {
            Passengers.Add(passenger);
            if (Passengers.Count == Capacity && Fill)
            {
                Fill = false;
            }
        }

        //======================================================================
        // ITransporter
        //======================================================================
        public Collection<Unit> Passengers { get; set; }
        public int Capacity { get { return hits * 2; } }
        public bool IsFull
        {
            get
            {
                return (Passengers.Count == Capacity);
            }
        }
        public bool Fill { get; set; }

        public void ConveyPassengers()
        {
            foreach (Unit passenger in Passengers)
            {
                passenger.x = x;
                passenger.y = y;

                // If we're carrying helicopters, they should bring their passengers
                if (passenger is ITransporter transporter)
                {
                    transporter.ConveyPassengers();
                }
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
