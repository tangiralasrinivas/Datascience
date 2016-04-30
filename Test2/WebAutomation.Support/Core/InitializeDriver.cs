using WebAutomation.Support.Setup;
using OpenQA.Selenium;
using TechTalk.SpecFlow;

namespace WebAutomation.Support.Core
{
    [Binding]
    public static class InitializeDriver
    {
        public static IWebDriver Driver;

        [BeforeTestRun]
        public static void BeforeTestRun()
        {
            Driver = new DriverFactory().CreateDriver();
        }

        [AfterTestRun]
        public static void AfterTestRun()
        {
            Driver.Quit();
        }
    }
}
