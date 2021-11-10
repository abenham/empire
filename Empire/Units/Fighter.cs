namespace Empire
{
    class Fighter : Unit, IAircraft, IPassenger
    {
        public Fighter(int id)
        {
            Id = id;
            type = UnitType.Fighter;
            hits = 1;
            damage = 1;
            Movement = 4;
            MaximumRange = 20;
            Range = MaximumRange;
        }

        public override string ToString()
        {
            return $"fighter #{Id} hits {hits} movement {Movement} range {Range}";
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
        // IAircraft interface
        //======================================================================
        public int MaximumRange { get; set; }
        public int Range { get; set; }
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
    }
}
