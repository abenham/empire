namespace Empire
{
    class Cruiser : Unit, ISentry
    {
        public Cruiser(int id)
        {
            Id = id;
            type = UnitType.Cruiser;
            hits = 8;
            strength = 8;
            damage = 1;
            Movement = 2;
        }

        public override string ToString()
        {
            return $"cruiser #{Id} hits {hits} movement {Movement} sentry {Sentry}";
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
