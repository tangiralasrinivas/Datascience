using System;
using System.Configuration;


namespace Selenium.Specifications.Core
{
    public abstract class PageBase
    {
        protected PageBase()
        {
        }

        // Consider change for by passing
        private const bool useimplicitValidation = true;
        
        protected void Init()
        {
            if (useimplicitValidation == Boolean.Parse(ConfigurationManager.AppSettings["ImplicitValidation"]))
            {
                WaitForPageToLoad();
                ValidatePageLoaded();
            }
        }

        protected virtual void ValidatePageLoaded() { }

        protected virtual void WaitForPageToLoad() { }
    }
}

