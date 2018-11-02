$(document).ready(function () {
  var pre = document.getElementsByTagName('pre');
  
  // add buttons to every pre element
  for (var i = 0; i < pre.length; i++) {
    //make sure that we want a copy button
    if (pre[i].className !== 'no-copy-button'){
      var p = document.createElement('p')
      pre[i].prepend(p);
      var p = document.createElement('p')
      pre[i].append(p);
      var span = document.createElement('span');
      span.className = 'copy-button';
      span.textContent = 'COPY';
      //add button to the code block
      pre[i].prepend(span);
    }
  };

  //set tooltip parameters
  $('.copy-button').tooltip({
    trigger: 'click',
    placement: 'bottom',
    hideDelay: 200
  });

  //show the tooltip
  function setTooltip(btn, message) {
    $(btn).attr('data-original-title', message)
      .tooltip('show');
  }

  //hide tooltip after a certain amount of time
  function hideTooltip(btn) {
    setTimeout(function() {
      $(btn).tooltip('hide');
    }, 750);
  }

  //manage events for clipboard
  var clipboard = new Clipboard('.copy-button', {
    target: function(trigger) {
      //get rid of text present so it isn't copied too
      //do this because all elements are together instead
      //of separate
      trigger.textContent = ''
      return trigger.parentNode;
    }
  });

  //follow up on our event if we are successful or fail
  clipboard.on('success', function(e) {
    //clear selection because we don't want to see that crap
    e.clearSelection();
    //reset original trigger button to normal text
    e.trigger.textContent = 'Copy'
    //let user know we copied code
    setTooltip(e.trigger, 'Copied!');
    hideTooltip(e.trigger);
  });

  //show errors in the console
  clipboard.on('error', function(e) {
      console.error('Action:', e.action);
      console.error('Trigger:', e.trigger);
      //reset original trigger button to normal text
      e.trigger.textContent = 'Copy'
  });

});