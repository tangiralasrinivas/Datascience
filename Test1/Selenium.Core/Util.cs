using OpenQA.Selenium;

namespace Selenium.Core
{
    public static class Util
    {


        public static string GetValue(this IWebElement webElement)
        {
            IJavaScriptExecutor e = Browser.Instance as IJavaScriptExecutor;
            return (string)e.ExecuteScript(string.Format("return $('#{0}').val();", webElement.GetAttribute("id")));
        }

    }
}
