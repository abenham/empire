using System.Collections.ObjectModel;

namespace Empire
{
    interface ITransporter
    {
        Collection<Unit> Passengers { get; set; }
        int Capacity { get; }
        bool IsFull { get; }
        bool Fill { get; set; }
        void ConveyPassengers();

        void AddPassenger(Unit passenger);
    }
}
