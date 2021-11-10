namespace Empire
{
    /*
     * Represents a unit that has a limited range
     */
    interface IAircraft
    {
        // The range remaining
        int Range { get; set; }

        // The maximum range when fully fueled
        int MaximumRange { get; set; }

        // Reset range to maximum
        void Refuel();

        // Consume fuel for a single move
        int ConsumeFuel();

        // Consume fuel when remaining on station
        int ConsumeFuel(int timeOnStation);
    }
}
