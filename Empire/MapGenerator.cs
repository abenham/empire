using System;
using System.Diagnostics;

namespace Empire
{
    class MapGenerator
    {
        private static int maximumSeaArea;
        private static int minimumSeaArea;

        //---------------------------------------------------------------------
        // Generate
        // ========
        //
        // Generate a new map
        //---------------------------------------------------------------------
        public static Map Generate()
        {
            Map map = new Map();

            bool isValid = false;
            int attempts = 0;

            // Loop until we have a validated map
            while (!isValid)
            {
                if (attempts++ > 1000) break;
                Debug.Write("Attempt " + attempts.ToString() + ": ");

                // Create oceans and landMasses
                map = GenerateBaseMap();

                // Check ocean and continent sizes
                if (ValidateBodies(map))
                {
                    map.CalculateBodyAreas();

                    // Check total sea area
                    if (ValidateSeaArea(map))
                    {
                        if (ValidateDensity(map))
                        {
                            // If we get here the map is valid
                            isValid = true;

                            // Generate the cities
                            GenerateCities(map);
                        }
                    }
                }
            }

            if (!isValid)
            {
                Console.WriteLine("Map generation abandoned");
                map = null;
            }

            return map;
        }

        //---------------------------------------------------------------------
        // GenerateBaseMap
        // ===============
        // 
        // Generates a basic map with no validation rules applied
        //---------------------------------------------------------------------
        private static Map GenerateBaseMap()
        {
            Random rand = new Random();
            Map map = new Map();
            int area = map.width * map.height;
            minimumSeaArea = area / 3;
            maximumSeaArea = area * 2 / 3;

            do
            {
                Map land = MakeLand();

                int mainY = rand.Next(map.height);
                int mainX = rand.Next(map.width);
                int landX;
                int landY;

                for (int l = 0; l < 39; l++)
                {
                    for (int m = 0; m < 39; m++)
                    {
                        if (land.Location(l, m).type != TerrainType.None)
                        {
                            landX = mainX + l - 20;
                            landY = mainY + m - 20;

                            if (landX >= 0 && landX < map.width)
                            {
                                if (landY >= 0 && landY < map.height)
                                {
                                    map.Location(landX, landY).type = TerrainType.Land;
                                }
                            }
                        }
                    }
                }

                area = GetSeaArea(map);
            } while (!(area >= minimumSeaArea && area <= maximumSeaArea));

            return map;
        }

        //---------------------------------------------------------------------
        // GenerateCities
        // ===============
        //
        // Places cities on the map
        //---------------------------------------------------------------------
        private static void GenerateCities(Map map)
        {
            Random rand = new Random();

            // Get map dimensions, initialise an ID for each city
            int width = map.width;
            int height = map.height;
            int id = 0;

            //
            // Number of cities to generate... 1/50th of the land area
            // But limit to somewhere between 52 and 70
            //
            int targetCities = 1 + (((width * height) - map.seaArea) / 50);
            targetCities = Math.Max(52, targetCities);
            targetCities = Math.Min(70, targetCities);

            //
            // 60% of cities are to be on the big ocean, and the rest are
            // to be inland
            //
            int coastalCities = (targetCities * 3 / 5) + rand.Next(12);
            int inlandCities = targetCities - coastalCities;
            int cityX;
            int cityY;

            //
            // For each required coastal city...
            //
            for (int n = 1; n < coastalCities; n++)
            {
                bool isValidLocation = false;

                do
                {
                    // Generate some random coordinates
                    cityX = 2 + rand.Next(width - 2);
                    cityY = 2 + rand.Next(height - 2);

                    // If the proposed location is on land...
                    if (map.Location(cityX, cityY).type == TerrainType.Land)
                    {
                        // If this land is on the shoreline of the big ocean...
                        if (map.IsOnShoreline(cityX, cityY))
                        {
                            // If this shoreline location has no neighbouring cities...
                            if (!map.HasNeighbours(cityX, cityY))
                            {
                                isValidLocation = true;
                            }
                        }
                    }
                } while (!isValidLocation);

                // Add it to the collection of cities
                map.AddCity(id, cityX, cityY, true);

                // Next city
                id++;
            }

            //
            // For each required inland city...
            //
            for (int n = 1; n < inlandCities; n++)
            {
                bool isValidLocation = false;

                do
                {
                    // Generate some random coordinates
                    cityX = 2 + rand.Next(width - 2);
                    cityY = 2 + rand.Next(height - 2);

                    // If the proposed location is on land...
                    if (map.Location(cityX, cityY).type == TerrainType.Land)
                    {
                        // If this location is landlocked...
                        if (map.IsLandLocked(cityX, cityY))
                        {
                            // If this land locked location has no neighbouring cities...
                            if (!map.HasNeighbours(cityX, cityY))
                            {
                                isValidLocation = true;
                            }
                        }
                    }
                } while (!isValidLocation);

                // add it to the collection of cities
                map.AddCity(id, cityX, cityY, false);

                // Next city
                id++;
            }
        }

        //---------------------------------------------------------------------
        // MakeLand
        // ========
        // 
        // Makes a "blob" of land to be added to the map being constructed 
        //---------------------------------------------------------------------
        private static Map MakeLand()
        {
            Random rand = new Random();
            Map land = new Map(39, 39, TerrainType.None);

            // Set the centre location to land, grow the land from this point
            land.Location(20, 20).type = TerrainType.Land;

            int vary = 2 + rand.Next(3);
            double radius = rand.Next(4) + rand.Next(3);
            int start = 90 - rand.Next(180);

            for (int rotation = start; rotation < start + 360; rotation += 3)
            {
                if (radius > 0.0)
                {
                    double cosAng = Math.Cos(rotation / Math.PI);
                    double sinAng = Math.Sin(rotation / Math.PI);
                    double rad = 0.0;
                    double diver = 0.5 / (Math.Abs(cosAng) + Math.Abs(sinAng));

                    while (rad <= radius)
                    {
                        rad += diver;
                        int x = (int)Math.Floor(20.0 + (rad * cosAng));
                        int y = (int)Math.Floor(20.0 + (rad * sinAng));

                        land.SetTerrain(x, y, TerrainType.Land);
                    }

                    if (rotation % 10.0 == 0.0)
                    {
                        radius += (rand.Next(vary) - (vary / 2));
                        if (vary % 2 == 0) radius += rand.Next(2);
                        if (radius >= 12) radius = 11;
                    }
                }
            }

            return land;
        }

        //---------------------------------------------------------------------
        // GetSeaArea
        // ==========
        //
        // Calculates the total area of the map that is sea
        //---------------------------------------------------------------------

        private static int GetSeaArea(Map map)
        {
            // Get total area of map that is sea.
            int area = 0;

            for (int x = 0; x < map.width; x++)
            {
                for (int y = 0; y < map.height; y++)
                {
                    if (map.Location(x, y).type == TerrainType.Sea)
                    {
                        area++;
                    }
                }
            }

            return area;
        }

        //---------------------------------------------------------------------
        // ValidateSeaArea
        // ===============
        //
        // Checks that there is not too much or too little water
        //---------------------------------------------------------------------
        private static bool ValidateSeaArea(Map map)
        {
            bool isValid = false;

            // Get total area of map that is sea.
            int area = GetSeaArea(map);

            // If not too dry or too wet...
            if (area >= minimumSeaArea && area <= maximumSeaArea)
            {
                map.seaArea = area;
                isValid = true;
            }
            else
            {
                Debug.WriteLine("Failed sea area check. Area is {0}, range is {1}-{2}", area, minimumSeaArea, maximumSeaArea);
            }

            return isValid;
        }

        //---------------------------------------------------------------------
        // ValidateBodies
        // ==============
        //
        // Checks the individual oceans and landMasses
        //---------------------------------------------------------------------
        private static bool ValidateBodies(Map map)
        {
            const int MAX_OCEAN_AREA = 12000;
            const int MAX_LAND_AREA = 1200;

            bool isValid = true;

            int bodyID = 0;

            // For each map location...
            for (int x = 0; x < map.width; x++)
            {
                for (int y = 0; y < map.height; y++)
                {
                    // If we have not identified this location as belonging to a body...
                    if (map.Location(x, y).body == -1)
                    {
                        // If this location is water...
                        if (map.Location(x, y).type == TerrainType.Sea)
                        {
                            // Identify this body of water, if the maximum area is exceeded
                            // fail this map build and start again
                            if (IdentifyBody
                                    (
                                        map,
                                        x,
                                        y,
                                        bodyID,
                                        TerrainType.Sea,
                                        MAX_OCEAN_AREA
                                    ))
                            {
                                // Ocean was good, so increment id for next one we encounter
                                Body body = new Body
                                {
                                    id = bodyID,
                                    isWater = true
                                };
                                map.AddBody(body);
                                bodyID++;
                                map.oceans++;
                            }
                            else
                            {
                                Debug.WriteLine("Failed large ocean test");
                                isValid = false;
                                break;
                            }
                        }
                        else
                        {
                            // Identify this land mass, if the maximum area is exceeded
                            // fail this map build and start again
                            if (IdentifyBody(map, x, y, bodyID, TerrainType.Land, MAX_LAND_AREA))
                            {
                                // Continent was good, so increment id for next one we encounter
                                Body body = new Body
                                {
                                    id = bodyID,
                                    isWater = false,
                                };
                                map.AddBody(body);
                                bodyID++;
                                map.landMasses++;
                            }
                            else
                            {
                                Debug.WriteLine("Failed large continent test");
                                isValid = false;
                                break;
                            }
                        }
                    }

                    if (!isValid) break;
                }

                if (!isValid) break;
            }

            return isValid;
        }

        //---------------------------------------------------------------------
        // ValidateDensity
        // ===============
        //
        // Checks that that are an appropriate number of landMasses and at 
        // least one large ocean.
        //---------------------------------------------------------------------
        private static bool ValidateDensity(Map map)
        {
            bool isValid = true;
            map.bigOcean = -1;

            // 
            // If the density of the landMasses is not too small
            // or too large...
            //
            if (map.landMasses >= 10 && map.landMasses <= 30)
            {
                //
                // We require one large ocean that takes 80 % of the wet area
                //
                // Loop through the oceans until we find the first big ocean
                //
                int bigOceanArea = map.seaArea * 4 / 5;

                foreach (Body body in map.bodies.Values)
                {
                    if (body.isWater && body.area >= bigOceanArea)
                    {
                        map.bigOcean = body.id;
                        break;
                    }
                }

                if (map.bigOcean == -1)
                {
                    Debug.WriteLine("No large ocean found.");
                    isValid = false;
                }
            }
            else
            {
                Debug.WriteLine("Continent density failure. {0} landMasses, range is 10-30", map.landMasses);
                isValid = false;
            }

            return isValid;
        }


        //---------------------------------------------------------------------
        // IdentifyBody
        // ============
        //
        // Crawls across the map to identify all locations that form a single
        // body.
        //---------------------------------------------------------------------
        private static bool IdentifyBody
                                    (
                                        Map map,
                                        int xStart,
                                        int yStart,
                                        int bodyID,
                                        TerrainType terrainType,
                                        int maximumArea
                                    )
        {
            int[] xOffset = new int[] { -1, 0, 1, -1, 1, -1, 0, 1 };
            int[] yOffset = new int[] { -1, -1, -1, 0, 0, 1, 1, 1 };
            int[] xStack = new int[map.height * map.width];
            int[] yStack = new int[map.height * map.width];
            int[] dStack = new int[map.height * map.width];
            int xMin = 0;
            int xMax = map.width - 1;
            int yMin = 0;
            int yMax = map.height - 1;

            map.Location(xStart, yStart).body = bodyID;
            int area = 0;
            int x = xStart;
            int y = yStart;
            int direction;
            int xExamine;
            int yExamine;
            Terrain location;

            bool finished = false;
            bool goodLocation;

            // Set the initial direction to examine (up and left)
            direction = 0;


            // Loop until we've identified this body completely
            while (!finished)
            {
                // Set the coords of the location to be examined
                xExamine = x + xOffset[direction];
                yExamine = y + yOffset[direction];
                goodLocation = false;

                //
                // Examine the terrain in the chosen direction, skip to the next location
                // if any of the following are true...
                //
                // The location is on either the left or right edge of the map
                // The location is on either the top or bottom edge of the map
                //
                if (xExamine >= xMin && xExamine <= xMax && yExamine >= yMin && yExamine <= yMax)
                {
                    //
                    // The coordinates are good so get this location from the map
                    //
                    location = map.Location(xExamine, yExamine);

                    // 
                    // If any of the following are true, skip to the next location...
                    //
                    // The location is not the specified terrain type
                    // The location already has a body ID assigned to it
                    //
                    if (location.type == terrainType && location.body == -1)
                    {
                        //
                        // Location passed all the tests, so assign the body ID to the examined location
                        // Save the coordinates of the source location, and the direction we just used
                        // Increment the index of where we're up to
                        //
                        goodLocation = true;
                        location.body = bodyID;
                        xStack[area] = x;
                        yStack[area] = y;
                        dStack[area] = direction;
                        area++;

                        //
                        // "area" is the number of locations examined, so if this exceeds the maximum
                        // body area, give up now
                        //
                        if (area >= maximumArea)
                        {
                            Debug.WriteLine("MapGenerator.IdentifyBody: Maximum area exceeded " + area.ToString());
                            return false;
                        }

                        //
                        // Use the examined location as the new source location, and examine
                        // the terrain from this new location
                        // Reset the direction to the initial up and left
                        //
                        x = xExamine;
                        y = yExamine;
                        direction = 0;
                    }
                }
                //
                // The examined location failed one of the tests
                //
                if (!goodLocation)
                {
                    bool newDirection = false;

                    while (!newDirection)
                    {
                        // Try the next direction from this point
                        direction++;

                        // If we've tried all directions...
                        if (direction >= xOffset.Length)
                        {
                            // Go back to the previous examined location
                            area--;

                            // If we're back to the orginal location, we have finished!
                            if (area <= 0)
                            {
                                finished = true;
                                newDirection = true;
                            }
                            // Otherwise, recover the coordinates and last used direction
                            else
                            {
                                x = xStack[area];
                                y = yStack[area];
                                direction = dStack[area];
                            }
                        }
                        else
                        {
                            newDirection = true;
                        }
                    }
                }
            }

            return true;
        }
    }
}
