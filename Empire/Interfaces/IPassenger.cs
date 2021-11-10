namespace Empire
{
    interface IPassenger
    {
        ITransporter EmbarkedOn { get; set; }
        bool Embarked { get; }
        void Embark(ITransporter transport);
        void Disembark();
    }
}
