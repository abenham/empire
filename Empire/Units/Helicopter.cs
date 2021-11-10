using System.Collections.ObjectModel;

namespace Empire
{
    class Helicopter : Unit, IPassenger, IAircraft, ITransporter
    {
        public Helicopter(int id)
        {
            Id = id;
            type = UnitType.Helicopter;
            hits = 1;
            damage = 1;
            Movement = 3;
            MaximumRange = 15;
            Range = MaximumRange;
            Passengers = new Collection<Unit>();
        }

        public override string ToString()
        {
            return $"helicopter #{Id} hits {hits} movement {Movement} range {Range} passengers {Passengers.Count} capacity {Capacity}";
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
        // IPassenger interface
        //======================================================================
        public ITransporter EmbarkedOn { get; set; }

        public bool Embarked
        {
            get { return (EmbarkedOn != null); }
        }

        public void Embark(ITransporter transport)
        {
            transport.AddPassenger(this);
            EmbarkedOn = transport;
        }

        public void Disembark()
        {
            EmbarkedOn.Passengers.Remove(this);
            EmbarkedOn = null;
        }

        //======================================================================
        // IAircraft
        //======================================================================
        public int Range { get; set; }
        public int MaximumRange { get; set; }

        public void Refuel()
        {
            Range = MaximumRange;
        }

        public int ConsumeFuel()
        {
            Range--;
            return Range;
        }

        public int ConsumeFuel(int timeOnStation)
        {
            Range -= timeOnStation;
            return Range;
        }

        //======================================================================
        // ITransporter
        //======================================================================
        public Collection<Unit> Passengers { get; set; }
        public int Capacity { get { return 1; } }
        public bool IsFull
        {
            get
            {
                return (Passengers.Count == 1);
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
    }
}
