namespace Empire
{
    class Army : Unit, IPassenger, ISentry
    {
        public Army(int id)
        {
            Id = id;
            type = UnitType.Army;
            hits = 1;
            damage = 1;
            Movement = 1;
        }

        public override string ToString()
        {
            return $"army #{Id} hits {hits} movement {Movement} sentry {Sentry}";
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
