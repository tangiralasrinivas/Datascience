using Selenium.Core;
using TechTalk.SpecFlow;

namespace Selenium.Specifications.Hooks
{
    [Binding]
    public static class InitializeBrowser
    {
        [BeforeFeature]
        public static void Init()
        {
            Browser.Init();
        }

        [AfterFeature]
        public static void Cleanup()
        {
            Browser.Close();
        }
    }
}
