using System.Collections.ObjectModel;

namespace Empire
{
    class AircraftCarrier : Unit, ITransporter, ISentry
    {
        public AircraftCarrier(int id)
        {
            Id = id;
            type = UnitType.AircraftCarrier;
            hits = 8;
            strength = 8;
            damage = 1;
            Movement = 2;
            Passengers = new Collection<Unit>();
        }

        public override string ToString()
        {
            string text;

            if (alignment == Alignment.Player)
            {
                text = $"aircraft carrier #{Id}, hits {hits}, movement {Movement}, passengers {Passengers.Count}, capacity {Capacity}, sentry {Sentry}";
            }
            else
            {
                text = $"Enemy aircraft carrier #{Id}";
            }

            return text;
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
