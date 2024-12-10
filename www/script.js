// The JavaScript code below runs the function that triggers a click "event" switching to the resepctive tabName when the link is clicked

function goToTab(tabName) {
        var tab = document.querySelector("a[data-value=" + tabName + "]");
        if (tab) {
          tab.click()
          // var tabUrl = tab.getAttribute("href");
          // window.open(tab, "_blank");
        }
      }