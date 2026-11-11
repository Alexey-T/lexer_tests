using System.Data;

namespace Burray
{
    public class HotWaterGuidedSchedulingScreen : CommonScreen
    {
        public void SetFirstScreen()
        {
        }

        public List<DayOfWeek> SelectedDaysOfWeek
        {
        }

        private static enum ScreensEnum
        {
            HVAC_GS1 = 0,
        }

        private class ScreenData
        {
            public bool HasButtons;
        }

        public static void hello()
        {
        }

    }
}
