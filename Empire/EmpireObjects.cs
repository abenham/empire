namespace Empire
{
    public enum Alignment
    {
        Neutral,
        Player,
        Enemy
    };

    public enum RootCommand
    {
        Edit,
        Exit,
        Help,
        Invalid,
        New,
        RefreshMap,
        Turn,
        DebugEnable,
        DebugPrintBody,
        DebugPrintEnemyMap,
        DebugSetDate,
        DebugList,
        DebugPrintMasterMap
    }

    public enum RootDebugListOption
    {
        Abort,
        Cities,
        Invalid,
        Units
    }
    public enum EditorAction
    {
        Exit,
        Fill,
        Invalid,
        Move,
        None,
        Production,
        Sentry
    }

    public struct EditorActionDetail
    {
        public EditorAction action;
        public int x;
        public int y;
    }
    public enum OrderType
    {
        MoveNorth,
        MoveNorthEast,
        MoveEast,
        MoveSouthEast,
        MoveSouth,
        MoveSouthWest,
        MoveWest,
        MoveNorthWest,
        None,
        Sentry,
        Fill
    }
}