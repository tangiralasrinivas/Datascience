using Selenium.Core;
using OpenQA.Selenium.Support.UI;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Selenium.Specifications.Hooks
{
    public static class StepsBase
    {
        private static readonly string localURL = "http://127.0.0.1:7777/portalserver/localhost/activate";
        private static readonly string devURL = "https://localhostcarddev/portalserver/localhost/index";
        private static readonly string testURL = "https://localhostcardtest.localhost.net/portalserver/localhost/index";
        private static readonly string prodURL = "https://norstromcard.com/portalserver/localhost/index";

        public static void NavigateTo()
        {
            Browser.Instance.Url = GetURL();
            WebDriverWait wait = new WebDriverWait(Browser.Instance, TimeSpan.FromSeconds(5));
            wait.Until(ExpectedConditions.TitleContains(Browser.Instance.Title));
        }

        private static string GetURL()
        {
            string environment = ConfigurationManager.AppSettings["Environment"];

            switch (environment)
            {
                case "Local":
                    return localURL;
                case "Development":
                    return devURL;
                case "Test":
                    return testURL;
                case "Production":
                    return prodURL;
                default:
                    return null;
            }
        }
    }
}
