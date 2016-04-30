using WebAutomation.Support.Core;
using WebAutomation.Support.Constants;

namespace WebAutomation.Support.Pages
{

    internal partial class SUTMainPage : PageBase
    {
        public SUTMainPage(PageContext context)
            : base(context)
        {
        }

        public void ClickOnLogo()
        {
            ClickById(IdAttribute.Logo);
        }
    }
}