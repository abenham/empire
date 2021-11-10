using System;
using System.Collections.Generic;
using System.Text;

namespace Empire
{
    class Presentation
    {
        private readonly string blankLine;

        public Presentation()
        {
            Console.SetWindowSize(Console.LargestWindowWidth, Console.LargestWindowHeight);
            Console.CursorSize = 100;
            blankLine = new string(' ', 120);
        }

        public void DisplayMessage(string message)
        {
            Console.SetCursorPosition(0, 0);
            Console.Write(blankLine);
            Console.SetCursorPosition(0, 0);
            Console.Write(message);
        }

        public void DisplayMessage(string message, ConsoleColor colour)
        {
            Console.SetCursorPosition(0, 0);
            Console.Write(blankLine);
            Console.SetCursorPosition(0, 0);
            Console.ForegroundColor = colour;
            Console.Write(message);
            Console.ForegroundColor = ConsoleColor.White;
        }

        public RootCommand GetRootCommand(bool AllowDebugCommands)
        {
            RootCommand command = RootCommand.Invalid;
            ConsoleColor promptColour = AllowDebugCommands ? ConsoleColor.Red : ConsoleColor.White;

            while (command == RootCommand.Invalid)
            {
                DisplayMessage("Empire> ", promptColour);

                ConsoleKeyInfo input = Console.ReadKey();

                switch (input.Key)
                {
                    // Display help
                    case ConsoleKey.H:
                    case ConsoleKey.Help:
                        command = RootCommand.Help;
                        break;

                    // Refresh map
                    case ConsoleKey.D:
                        command = RootCommand.RefreshMap;
                        break;

                    // Editor
                    case ConsoleKey.E:
                        command = RootCommand.Edit;
                        break;

                    // New game
                    case ConsoleKey.N:
                        command = RootCommand.New;
                        break;

                    // Begin turn
                    case ConsoleKey.T:
                        command = RootCommand.Turn;
                        break;

                    // Controlled exit
                    case ConsoleKey.Escape:
                        command = RootCommand.Exit;
                        break;

                    // Debugging commands...
                    case ConsoleKey.F3:
                        command = RootCommand.DebugEnable;
                        break;

                    case ConsoleKey.F4:
                        if (AllowDebugCommands) command = RootCommand.DebugPrintBody;
                        break;

                    case ConsoleKey.F5:
                        if (AllowDebugCommands) command = RootCommand.DebugPrintEnemyMap;
                        break;

                    case ConsoleKey.F6:
                        if (AllowDebugCommands) command = RootCommand.DebugSetDate;
                        break;

                    case ConsoleKey.L:
                        if (AllowDebugCommands) command = RootCommand.DebugList;
                        break;

                    case ConsoleKey.M:
                        if (AllowDebugCommands) command = RootCommand.DebugPrintMasterMap;
                        break;
                }
            }

            return command;
        }

        public RootDebugListOption GetDebugListOption()
        {
            RootDebugListOption option = RootDebugListOption.Invalid;

            while (option == RootDebugListOption.Invalid)
            {
                DisplayMessage("List> ");

                ConsoleKeyInfo input = Console.ReadKey();

                switch (input.Key)
                {
                    case ConsoleKey.C:
                        option = RootDebugListOption.Cities;
                        break;

                    case ConsoleKey.U:
                        option = RootDebugListOption.Units;
                        break;

                    case ConsoleKey.Escape:
                        option = RootDebugListOption.Abort;
                        break;
                }
            }

            return option;
        }

        public void DisplayMap(Map map)
        {
            Console.Clear();

            for (int y = 0; y < map.height; y++)
            {
                Console.ForegroundColor = ConsoleColor.White;
                Console.SetCursorPosition(0, y + 1);
                Console.Write(y);

                for (int x = 0; x < map.width; x++)
                {
                    Terrain location = map.Location(x, y);

                    if (location.type != TerrainType.None)
                    {
                        Console.SetCursorPosition(x + 8, y + 1);
                        Console.ForegroundColor = PickTerrainColour(location);
                        Console.Write(location.ToString());
                    }
                }
            }

            Console.ResetColor();
            Console.SetCursorPosition(0, 0);
        }

        private ConsoleColor PickTerrainColour(Terrain location)
        {
            ConsoleColor colour = ConsoleColor.White;

            if (location.occupant == null)
            {
                switch (location.type)
                {
                    case TerrainType.Sea:
                        colour = ConsoleColor.DarkCyan;
                        break;

                    case TerrainType.Land:
                        colour = ConsoleColor.DarkGreen;
                        break;

                    case TerrainType.City:
                        colour = ConsoleColor.Red;
                        break;
                }
            }
            return colour;
        }

        public void DisplayArea(Map map, int xCentre, int yCentre, int border)
        {
            Console.Clear();
            Console.SetCursorPosition(0, 1);

            for (int y = Math.Max(0, yCentre - border); y < Math.Min(yCentre + border, map.height); y++)
            {
                StringBuilder line = new StringBuilder();

                line.Append(y.ToString());
                line.Append("\t");

                Console.ForegroundColor = ConsoleColor.White;
                Console.Write(line.ToString());

                for (int x = Math.Max(0, xCentre - border); x < Math.Min(xCentre + border, map.width); x++)
                {
                    Console.ForegroundColor = PickTerrainColour(map.Location(x, y));
                    Console.Write(map.Location(x, y).ToString());
                }

                Console.WriteLine();
            }

            Console.ResetColor();
        }

        public void UpdateAdjacent(Map map, int xCentre, int yCentre)
        {
            int x;
            int y;

            for (int dy = -1; dy <= 1; dy++)
            {
                y = yCentre + dy;

                for (int dx = -1; dx <= 1; dx++)
                {
                    x = xCentre + dx;

                    if (x >= 0 && x < map.width && y >= 0 && y < map.height)
                    {
                        Terrain location = map.Location(x, y);
                        Console.ForegroundColor = PickTerrainColour(location);
                        Console.SetCursorPosition(x + 8, y + 1);
                        Console.Write(location.ToString());
                    }
                }
            }

            Console.ForegroundColor = ConsoleColor.White;
        }

        public int GetIntegerValue(String prompt)
        {
            bool isValid;
            int value;

            do
            {
                DisplayMessage(prompt);
                String input = Console.ReadLine();
                isValid = int.TryParse(input, out value);
            } while (!isValid);

            return value;
        }

        public int GetNewGameDate(int currentDate)
        {
            String prompt = currentDate.ToString() + " new date? ";
            return GetIntegerValue(prompt);
        }

        public bool ConfirmExit()
        {
            DisplayMessage("Exit: Are you sure? ");
            ConsoleKeyInfo input = Console.ReadKey();

            return (input.Key == ConsoleKey.Y);
        }
        public bool ConfirmNewGame()
        {
            DisplayMessage("New Game: This will discard the existing game. Are you sure? ");
            ConsoleKeyInfo input = Console.ReadKey();

            return (input.Key == ConsoleKey.Y);
        }
        public void ShowAndWait(string message)
        {
            DisplayMessage(message);
            Console.Write("\nPress any key to continue...");
            _ = Console.ReadKey();
        }
        public void ShowHelp()
        {
            Console.Clear();
            Console.WriteLine("Empire version 5.0");
            Console.WriteLine("------------------");
            Console.WriteLine();
            Console.WriteLine("D\tDisplay map");
            Console.WriteLine("E\tEnd turn");
            Console.WriteLine("H\tThis help text");
            Console.WriteLine("O\tGive orders");
            Console.WriteLine("N\tCreate a new map");
            Console.WriteLine("X\tExit the game");
            Console.WriteLine();

            Console.WriteLine("Debugging...");
            Console.WriteLine("+\tEnable and disable these debugging commands");
            Console.WriteLine("b\tDisplay ocean and land mass map");
            Console.WriteLine("c\tDisplay console information");
            Console.WriteLine("m\tDisplay master map");
            Console.WriteLine("s\tDisplay map statistics");

            Console.Write("\nPress any key to continue...");
            Console.ReadKey(true);

            Console.Clear();
        }

        public void ListCities(Dictionary<int, City> cities)
        {
            Console.Clear();
            Console.WriteLine("City Details");
            Console.WriteLine("X\tY\tAlignment\tProduction\tCompletion");

            foreach (City city in cities.Values)
            {
                Console.WriteLine("{0}\t{1}\t{2}\t\t{3}\t\t{4}", city.x, city.y, city.alignment.ToString(), city.Production.ToString(), city.Completion);
            }

            Console.Write("\nPress any key to continue...");
            _ = Console.ReadKey();
            Console.Clear();
        }

        public void ListUnits(Dictionary<int, Unit> units)
        {
            Console.Clear();
            Console.WriteLine("Unit Details");

            foreach (Unit unit in units.Values)
            {
                Console.WriteLine($"{unit.x}\t{unit.y}\t{unit.Destroyed}\t{unit.ToString()}");
            }

            Console.Write("\nPress any key to continue...");
            _ = Console.ReadKey();
            Console.Clear();
        }

        public void PrintBody(Map map)
        {
            string asciichar;

            for (int h = 0; h < map.height; h++)
            {
                StringBuilder line = new StringBuilder();
                line.Append(h.ToString());
                line.Append("\t");

                for (int w = 0; w < map.width; w++)
                {
                    asciichar = (Convert.ToChar(48 + map.Location(w, h).body)).ToString();
                    line.Append(asciichar);
                }

                Console.WriteLine(line.ToString());
            }
        }

        //---------------------------------------------------------------------
        // ChooseCityProduction
        // ====================
        //
        // Ask the player to choose production for a city
        //---------------------------------------------------------------------
        public UnitType ChooseCityProduction(Map map, City city)
        {
            UnitType production = UnitType.None;

            int x = Console.CursorLeft;
            int y = Console.CursorTop;

            Console.SetCursorPosition(0, 100);

            do
            {
                DisplayArea(map, city.x, city.y, 10);

                Console.WriteLine("What are your production demands for this city?");
                Console.WriteLine("\tA - Army");
                Console.WriteLine("\tF - Fighter");
                Console.WriteLine("\tH - Helicopter");

                if (!city.landLocked)
                {
                    Console.WriteLine("\tD - Destroyer");
                    Console.WriteLine("\tS - Submarine");
                    Console.WriteLine("\tR - Cruiser");
                    Console.WriteLine("\tB - Battleship");
                    Console.WriteLine("\tC - Aircraft Carrier");
                    Console.WriteLine("\tL - Amphibious Assault Ship");
                    Console.WriteLine("\tT - Troop Transport");
                }

                Console.Write("\nChoice: ");

                ConsoleKeyInfo input = Console.ReadKey();

                switch (input.KeyChar.ToString().ToUpper())
                {
                    case "A":
                        production = UnitType.Army;
                        break;

                    case "F":
                        production = UnitType.Fighter;
                        break;

                    case "H":
                        production = UnitType.Helicopter;
                        break;
                }

                if (!city.landLocked && production == UnitType.None)
                {
                    switch (input.Key)
                    {
                        case ConsoleKey.D:
                            production = UnitType.Destroyer;
                            break;

                        case ConsoleKey.S:
                            production = UnitType.Submarine;
                            break;

                        case ConsoleKey.R:
                            production = UnitType.Cruiser;
                            break;

                        case ConsoleKey.B:
                            production = UnitType.Battleship;
                            break;

                        case ConsoleKey.C:
                            production = UnitType.AircraftCarrier;
                            break;

                        case ConsoleKey.L:
                            production = UnitType.AssaultShip;
                            break;

                        case ConsoleKey.T:
                            production = UnitType.TroopTransport;
                            break;
                    }
                }
            } while (production == UnitType.None);

            Console.SetCursorPosition(x, y);

            return production;
        }

        //---------------------------------------------------------------------
        // Editor
        // =========
        //
        // Between turnns, allows the player to make changes to cities, etc
        //---------------------------------------------------------------------
        public void Editor(Game game, Map map)
        {
            int x = Console.WindowWidth / 2;
            int y = Console.WindowHeight / 2;
            bool exit = false;

            Terrain location;

            DisplayMap(map);
            Console.SetCursorPosition(x + 8, y + 1);

            while (!exit)
            {
                ConsoleKeyInfo key = Console.ReadKey(true);

                switch (key.Key)
                {
                    // Move west
                    case ConsoleKey.A:
                        if (EditorMoveCursor(map, x, y, -1, 0))
                        {
                            x--;
                        }
                        break;

                    // Move north west
                    case ConsoleKey.Q:
                        if (EditorMoveCursor(map, x, y, -1, -1))
                        {
                            x--;
                            y--;
                        }
                        break;

                    // Move north
                    case ConsoleKey.W:
                        if (EditorMoveCursor(map, x, y, 0, -1))
                        {
                            y--;
                        }
                        break;

                    // Move north east
                    case ConsoleKey.E:
                        if (EditorMoveCursor(map, x, y, 1, -1))
                        {
                            x++;
                            y--;
                        }
                        break;

                    // Move east
                    case ConsoleKey.D:
                        if (EditorMoveCursor(map, x, y, 1, 0))
                        {
                            x++;
                        }
                        break;

                    // Move south east
                    case ConsoleKey.C:
                        if (EditorMoveCursor(map, x, y, 1, 1))
                        {
                            x++;
                            y++;
                        }
                        break;

                    // Move south
                    case ConsoleKey.X:
                        if (EditorMoveCursor(map, x, y, 0, 1))
                        {
                            y++;
                        }
                        break;

                    // Move south-west
                    case ConsoleKey.Z:
                        if (EditorMoveCursor(map, x, y, -1, 1))
                        {
                            x--;
                            y++;
                        }
                        break;

                    // Change production
                    case ConsoleKey.P:
                        location = map.Location(x, y);
                        if (location.type == TerrainType.City && location.city.alignment == Alignment.Player)
                        {
                            game.ChooseCityProduction(location.city);
                            DisplayMap(map);
                            Console.SetCursorPosition(x + 8, y + 1);
                        }
                        break;

                    // Toggle sentry mode
                    case ConsoleKey.G:
                        {
                            location = map.Location(x, y);
                            if (location.occupant != null && location.occupant.alignment == Alignment.Player && location.occupant is ISentry sentry)
                            {
                                sentry.ToggleSentry();
                            }
                        }
                        break;

                    case ConsoleKey.Escape:
                        exit = true;
                        break;
                }
            }
        }

        public bool EditorMoveCursor(Map map, int x, int y, int dx, int dy)
        {
            bool validMove = map.EdgeCheck(x, y, dx, dy);

            if (validMove)
            {
                x += dx;
                y += dy;

                Terrain location = map.Location(x, y);

                if (location.type == TerrainType.City)
                {
                    DisplayMessage(location.city.ToString());
                }

                // Unoccupied location
                else if (location.occupant == null)
                {
                    DisplayMessage(location.type.ToString());
                }

                // Occupied location
                else
                {
                    DisplayMessage(location.occupant.ToString());
                }

                Console.SetCursorPosition(8 + x, y + 1);
            }

            return validMove;
        }

        public OrderType GetOrders(int x, int y)
        {
            OrderType orders = OrderType.None;
            bool isValid;

            do
            {
                Console.SetCursorPosition(8 + x, 1 + y);
                ConsoleKeyInfo input = Console.ReadKey(true);
                isValid = true;

                switch (input.Key)
                {
                    // Move west
                    case ConsoleKey.A:
                        orders = OrderType.MoveWest;
                        break;

                    // Move north west
                    case ConsoleKey.Q:
                        orders = OrderType.MoveNorthWest;
                        break;

                    // Move north
                    case ConsoleKey.W:
                        orders = OrderType.MoveNorth;
                        break;

                    // Move north east
                    case ConsoleKey.E:
                        orders = OrderType.MoveNorthEast;
                        break;

                    // Move east
                    case ConsoleKey.D:
                        orders = OrderType.MoveEast;
                        break;

                    // Move south east
                    case ConsoleKey.C:
                        orders = OrderType.MoveSouthEast;
                        break;

                    // Move south
                    case ConsoleKey.X:
                        orders = OrderType.MoveSouth;
                        break;

                    // Move south-west
                    case ConsoleKey.Z:
                        orders = OrderType.MoveSouthWest;
                        break;

                    // Stay at current location
                    case ConsoleKey.S:
                        orders = OrderType.None;
                        break;

                    // Fill - sleep until full
                    case ConsoleKey.F:
                        orders = OrderType.Fill;
                        break;

                    // Guard - turn on sentry mode
                    case ConsoleKey.G:
                        orders = OrderType.Sentry;
                        break;

                    default:
                        isValid = false;
                        break;
                }
            } while (!isValid);

            return orders;
        }

        public void Alert()
        {
            Console.Beep();
        }
    }
}
