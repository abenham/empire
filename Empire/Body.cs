namespace Empire
{
    class Body
    {
        public int id;
        public int area;
        public bool isWater;
        public int cityCount;
        public int portCount;
        public int score;

        public Body()
        {
            id = -1;
            area = 0;
            isWater = true;
            cityCount = 0;
            portCount = 0;
            score = 0;
        }
    }

}
