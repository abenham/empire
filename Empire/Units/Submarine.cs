namespace Empire
{
    class Submarine : Unit, ISentry
    {
        public Submarine(int id)
        {
            Id = id;
            type = UnitType.Submarine;
            hits = 2;
            strength = 2;
            damage = 2;
            Movement = 2;
        }

        public override string ToString()
        {
            return $"submarine #{Id} hits {hits} movement {Movement} sentry {Sentry}";
        }

        // ISentry
        public bool Sentry { get; set; }

        public bool ToggleSentry()
        {
            Sentry = !Sentry;
            return Sentry;
        }
    }
}
