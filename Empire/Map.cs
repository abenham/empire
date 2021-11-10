using System;
using System.Collections.Generic;

namespace Empire
{
    class Map
    {
        public const int DEFAULT_WIDTH = 180;
        public const int DEFAULT_HEIGHT = 60;
        public int width;
        public int height;
        public int oceans;
        public int landMasses;
        public int bigOcean;
        public int seaArea;

        public Dictionary<int, Body> bodies;
        public Dictionary<int, City> cities;
        private readonly Terrain[,] map;

        /*
         * Create a new map using the default 100x60 size. This is the main game map
        */
        public Map()
        {
            width = DEFAULT_WIDTH;
            height = DEFAULT_HEIGHT;
            oceans = 0;
            landMasses = 0;
            map = new Terrain[width, height];
            bodies = new Dictionary<int, Body>();
            cities = new Dictionary<int, City>();
            Initialise(TerrainType.Sea);
        }

        /*
        * Create a new map using a specified width and height. Used when generating
        * terrain for the main map.
        */
        public Map(int width, int height, TerrainType type)
        {
            this.width = width;
            this.height = height;
            oceans = 0;
            landMasses = 0;
            map = new Terrain[this.width, this.height];
            Initialise(type);
        }

        /*
        * Fill the map with the specified terrain type
        */
        public void Initialise(TerrainType type)
        {
            for (int w = 0; w < this.width; w++)
            {
                for (int h = 0; h < this.height; h++)
                {
                    map[w, h] = new Terrain(type);
                }
            }
        }

        /*
         * Get the terrain type at a set of coordinates
         */
        public Terrain Location(int x, int y)
        {
            return map[x, y];
        }

        public void SetTerrain(int x, int y, TerrainType type)
        {
            map[x, y].type = type;
        }

        public int SeaFrontage(int x, int y)
        {
            int frontage = 0;
            int adjacentX;
            int adjacentY;

            for (int dx = -1; dx <= 1; dx++)
            {
                adjacentX = x + dx;

                for (int dy = -1; dy <= 1; dy++)
                {
                    adjacentY = y + dy;

                    if (adjacentX >= 0 && adjacentX < width && adjacentY >= 0 && adjacentY < height)
                    {
                        if (!(adjacentX == x && adjacentY == y))
                        {
                            if (map[adjacentX, adjacentY].type == TerrainType.Sea)
                            {
                                frontage++;
                            }
                        }
                    }
                    else
                        frontage++;
                }
            }

            return frontage;
        }

        public void AddBody(Body body)
        {
            bodies.Add(body.id, body);
        }

        //---------------------------------------------------------------------
        // AddCity
        // =======
        //
        // Add a city to the collection at the specified location
        //---------------------------------------------------------------------
        public void AddCity(int id, int x, int y, bool isPort)
        {
            City city = new City(x, y);

            cities.Add(id, city);

            // Determine if the city is land locked
            // Get the map location of this city
            // Set it to be a city
            // Place the city on the map
            Terrain location = Location(city.x, city.y);
            city.landLocked = IsLandLocked(city.x, city.y);
            location.type = TerrainType.City;
            location.city = city;

            // Get the land mass for this location
            // Increase its score
            // Increase the count of non-port cities
            Body body = bodies[location.body];
            body.score += isPort ? 100 : 1;
            if (isPort)
                body.portCount++;

            body.cityCount++;
        }

        public void CalculateBodyAreas()
        {
            foreach (Terrain location in map)
            {
                bodies[location.body].area++;
            }
        }

        //---------------------------------------------------------------------
        // IsOnShoreline
        // =============
        //
        // Determines if the specified coordinates are on the shore of the big
        // ocean.
        //---------------------------------------------------------------------
        public bool IsOnShoreline(int cityX, int cityY)
        {
            bool isOnShoreline = false;
            int maxX;
            int minX;
            int maxY;
            int minY;

            //
            // Examine adjacent locations. We need one to be on the big ocean
            //
            minX = Math.Max(1, cityX - 1);
            maxX = Math.Min(width - 1, cityX + 1);
            minY = Math.Max(1, cityY - 1);
            maxY = Math.Min(height - 1, cityY + 1);

            for (int x = minX; x < maxX; x++)
            {
                for (int y = minY; y < maxY; y++)
                {
                    if (Location(x, y).body == bigOcean)
                    {
                        isOnShoreline = true;
                        break;
                    }
                }

                if (isOnShoreline) break;
            }

            return isOnShoreline;
        }

        //---------------------------------------------------------------------
        // HasNeighbours
        // =============
        //
        // Determines if the specified coordinates are close to a city
        //---------------------------------------------------------------------
        public bool HasNeighbours(int cityX, int cityY)
        {
            bool hasNeighbours = false;
            int maxX;
            int minX;
            int maxY;
            int minY;

            //
            // Examine nearby locations.
            //
            minX = Math.Max(1, cityX - 3);
            maxX = Math.Min(width - 1, cityX + 3);
            minY = Math.Max(1, cityY - 3);
            maxY = Math.Min(height - 1, cityY + 3);

            for (int x = minX; x < maxX; x++)
            {
                for (int y = minY; y < maxY; y++)
                {
                    // If there's a city nearby then we have neighbours, so stop searching
                    if (Location(x, y).type == TerrainType.City)
                    {
                        hasNeighbours = true;
                        break;
                    }
                }

                if (hasNeighbours) break;
            }

            return hasNeighbours;
        }

        //---------------------------------------------------------------------
        // IsLandLocked
        // ============
        //
        // Determines if the specified coordinates are land locked
        //---------------------------------------------------------------------
        public bool IsLandLocked(int cityX, int cityY)
        {
            bool isLandLocked = true;
            int maxX;
            int minX;
            int maxY;
            int minY;

            //
            // Examine adjacent locations. Each needs to be land
            //
            minX = Math.Max(1, cityX - 1);
            maxX = Math.Min(width - 1, cityX + 1);
            minY = Math.Max(1, cityY - 1);
            maxY = Math.Min(height - 1, cityY + 1);

            for (int x = minX; x < maxX; x++)
            {
                for (int y = minY; y < maxY; y++)
                {
                    if (Location(x, y).type != TerrainType.Land)
                    {
                        isLandLocked = false;
                        break;
                    }
                }

                if (!isLandLocked) break;
            }

            return isLandLocked;
        }

        public bool EdgeCheck(int x, int y, int dx, int dy)
        {
            bool ok = true;

            if (x + dx < 0 || x + dx == this.width || y + dy < 0 || y + dy == this.height)
            {
                ok = false;
            }

            return ok;
        }
    }
}
