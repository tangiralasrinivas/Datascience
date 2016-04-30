using BoDi;
using TechTalk.SpecFlow;
using WebAutomation.Support.Core;

namespace WebAutomation.Support.Hooks
{
    [Binding]
    public class DriverSetup
    {
        private readonly IObjectContainer objectContainer;

        public DriverSetup(IObjectContainer objectContainer)
        {
            this.objectContainer = objectContainer;
        }

        [BeforeScenario]
        public void BeforeScenario()
        {
            this.objectContainer.RegisterInstanceAs(new PageContext(InitializeDriver.Driver));
        }

        [AfterScenario]
        public void AfterScenario()
        {
        }
    }
}
