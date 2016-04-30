using OpenQA.Selenium;
using OpenQA.Selenium.Interactions;

namespace WebAutomation.Support.Core
{
    public class PageContext
    {
        public PageContext(IWebDriver driver)
        {
            this.Driver = driver;
        }

        public IWebDriver Driver { get; private set; }

        public Actions Actions { get; set; }
    }
}