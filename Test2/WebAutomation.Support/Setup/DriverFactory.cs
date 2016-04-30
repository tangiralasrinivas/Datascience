using WebAutomation.Support.Configuration;
using WebAutomation.Support.Core;
using OpenQA.Selenium;

namespace WebAutomation.Support.Setup
{
    /// <summary>
    /// Calls the WebDriver factory by creating required configuration based on the remote value.
    /// </summary>
    public class DriverFactory
    {
        public IWebDriver CreateDriver()
        {
            //If remote is true, then create a RemoteDriverConfig and pass it to the factory
            if (Configuration.Remote)
            {
                return new WebDriverFactory().Create(
                    new RemoteDriverConfiguration(
                            Configuration.Browser,
                            Configuration.Platform,
                            Configuration.BrowserVersion,
                            Configuration.SeleniumHubUrl,
                            Configuration.SeleniumHubPort));
            }

            //Else (false) create a LocalDriverConfig and pass this to the factory
            return new WebDriverFactory().Create(
                new LocalDriverConfiguration(
                    Configuration.Browser));
        }

    }
}
