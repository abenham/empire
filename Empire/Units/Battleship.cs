namespace Empire
{
    class Battleship : Unit, ISentry
    {
        public Battleship(int id)
        {
            Id = id;
            type = UnitType.Battleship;
            hits = 12;
            strength = 12;
            damage = 1;
            Movement = 2;
        }

        public override string ToString()
        {
            return $"battleship #{Id} hits {hits} movement {Movement} sentry {Sentry}";
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
