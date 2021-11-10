namespace Empire
{
    class Destroyer : Unit, ISentry
    {
        public Destroyer(int id)
        {
            Id = id;
            type = UnitType.Destroyer;
            hits = 3;
            strength = 3;
            damage = 1;
            Movement = 3;
        }

        public override string ToString()
        {
            return $"destroyer #{Id} hits {hits} movement {Movement} sentry {Sentry}";
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
