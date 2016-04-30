using System;
using OpenQA.Selenium;
using OpenQA.Selenium.Chrome;
using OpenQA.Selenium.Remote;
using System.Configuration;
using OpenQA.Selenium.Support.UI;

namespace Selenium.Core
{
    public class Browser
    {
        public static IWebDriver Instance { get; set; }


        public static void Init()
        {
            string browser = ConfigurationManager.AppSettings["browser"];

            if (bool.Parse(ConfigurationManager.AppSettings["runRemote"]))
            {
                DesiredCapabilities capabilities = null;
                switch (browser)
                {
                    case "chrome":
                        capabilities = DesiredCapabilities.Chrome();
                        break;
                    case "edge":
                        capabilities = DesiredCapabilities.Edge();
                        capabilities.SetCapability("pageLoadingStrategy", "eager");
                        capabilities.IsJavaScriptEnabled = true;
                        break;
                }
                Instance = new RemoteWebDriver(new Uri(ConfigurationManager.AppSettings["remoteURL"]), capabilities);
            }
            else
            {
                switch (browser)
                {
                    case "chrome":
                        var chromeDriverService = ChromeDriverService.CreateDefaultService();
                        chromeDriverService.HideCommandPromptWindow = true;
                        var options = new ChromeOptions();
                        options.AddArguments("start-maximized");
                        Instance = new ChromeDriver(chromeDriverService, options);
                        break;
                    default:
                        break;
                }
            }
            TurnOnWait();
        }


        public static void Close()
        {
            Instance.Close();
        }

        public static void TurnOffWait()
        {
            Instance.Manage().Timeouts().ImplicitlyWait(TimeSpan.FromMilliseconds(50));
        }

        public static void TurnOnWait()
        {
            Instance.Manage().Timeouts().ImplicitlyWait(TimeSpan.FromSeconds(double.Parse(ConfigurationManager.AppSettings["timeoutSecs"])));
        }

        private static void WaitUntil<T>(Func<IWebDriver, T> condition)
        {
            WaitUntil(condition, Int32.Parse(ConfigurationManager.AppSettings["timeoutSecs"]));
        }

        private static void WaitUntil<T>(Func<IWebDriver, T> condition, int waitTimeoutInSeconds)
        {
            var wait = new WebDriverWait(Instance, new TimeSpan(0, 0, 0, waitTimeoutInSeconds));
            wait.Until(condition);
        }

        public static IWebElement WaitForElement(By by)
        {
            return WaitForElement(by, Int32.Parse(ConfigurationManager.AppSettings["timeoutSecs"]));
        }

        private static IWebElement WaitForElement(By by, int timeout)
        {
            try
            {
                WaitUntil(x => x.FindElements(by).Count > 0, timeout);
                return Instance.FindElement(by);
            }
            catch (StaleElementReferenceException)
            {
                return WaitForElement(by, timeout);
            }
            catch (InvalidSelectorException e)
            {
                throw e;
            }
            catch (Exception)
            {
                string message =
                    string.Format("WaitForElement failed after {0} seconds, element should be present : {1}", timeout,
                                  by);
                throw new NoSuchElementException(message);
            }
        }
    }
}
