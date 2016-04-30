using OpenQA.Selenium.Chrome;

namespace Selenium.Core
{
    public class ActivationSeleniumFramework
    {

        public ChromeDriver GetIDBrowser()
        {
            var driver = new ChromeDriver();
            driver.Navigate().GoToUrl("http://127.0.0.1:7777/portalserver/localhost/activate");

            return driver;

        }

        public void OpenIdAndClose()
        {
            var driver = new ChromeDriver();
            driver.Navigate().GoToUrl("http://127.0.0.1:7777/portalserver/localhost/activate");

            driver.Dispose();

        }

        public void OpenAndLeaveOpen()
        {
            var driver = new ChromeDriver();
            driver.Navigate().GoToUrl("http://127.0.0.1:7777/portalserver/localhost/activate");

        }


    }
}
