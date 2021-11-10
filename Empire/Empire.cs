namespace Empire
{
    class Empire
    {
        static Game game;
        static bool allowDebugCommands = false;
        static readonly Presentation presentation = new Presentation();

        static void Main()
        {
            bool exit = false;
            RootCommand command;

            while (!exit)
            {
                command = presentation.GetRootCommand(allowDebugCommands);

                switch (command)
                {
                    case RootCommand.Edit:
                        if (game != null)
                        {
                            presentation.Editor(game, game.playerMap);
                        }
                        break;

                    case RootCommand.Exit:
                        exit = presentation.ConfirmExit();
                        break;

                    case RootCommand.Help:
                        presentation.ShowHelp();
                        break;

                    case RootCommand.New:
                        NewGame();
                        break;

                    case RootCommand.RefreshMap:
                        DisplayMap();
                        break;

                    case RootCommand.Turn:
                        if (game != null)
                        {
                            presentation.DisplayMap(game.playerMap);
                            game.PerformTurn();
                        }
                        break;

                    case RootCommand.DebugEnable:
                        allowDebugCommands = !allowDebugCommands;
                        break;

                    case RootCommand.DebugPrintBody:
                        if (allowDebugCommands && game != null)
                        {
                            presentation.PrintBody(game.map);
                        }
                        break;

                    case RootCommand.DebugPrintEnemyMap:
                        if (allowDebugCommands && game != null)
                        {
                            presentation.DisplayMap(game.enemyMap);
                        }
                        break;

                    case RootCommand.DebugSetDate:
                        if (allowDebugCommands && game != null)
                        {
                            game.GameDate = presentation.GetNewGameDate(game.GameDate);
                        }
                        break;

                    case RootCommand.DebugList:
                        if (allowDebugCommands && game != null)
                        {
                            switch (presentation.GetDebugListOption())
                            {
                                // List cities
                                case RootDebugListOption.Cities:
                                    presentation.ListCities(game.map.cities);
                                    break;

                                case RootDebugListOption.Units:
                                    presentation.ListUnits(game.units);
                                    break;
                            }
                        }
                        break;

                    case RootCommand.DebugPrintMasterMap:
                        if (allowDebugCommands && game != null)
                        {
                            presentation.DisplayMap(game.map);
                        }
                        break;
                }
            }
        }

        static void NewGame()
        {
            if (game == null || presentation.ConfirmNewGame())
            {
                game = new Game(presentation);

                if (game.Create())
                {
                    game.ChooseCityProduction(game.PlayerCity);
                }
                else
                {
                    presentation.ShowAndWait("Failed to generate game");
                }
            }
        }

        static void DisplayMap()
        {
            if (game != null)
                presentation.DisplayMap(game.playerMap);
        }
    }
}
