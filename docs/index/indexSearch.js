//search function
$(document).ready(function () {
  //get search box
  var searchBox = $("#searchInput")

  // get div for search results
  var resultdiv = $("#searchResults");

  //set search input as selected then loading page
  searchBox.focus();

  // Set up search
  var index, store;

  // Create index
  index = lunr.Index.load(indexJSON.index);
  
  //validate the index
  //this is for the purpose of debugging and it will be output to
  //the console if there are any problems
  Object.keys(index.documentStore.store)
  .forEach(function (documentId) {
    var documentTokens = index.documentStore.store[documentId];

    documentTokens.forEach(function (token) {
      if (index.corpusTokens.indexOf(token) == -1) {
        console.log('not in corpus tokens', documentId, token);
      }

      if (!index.tokenStore.has(token)) {
        console.log('not in token store', documentId, token);
      }
    });
  });

  // Create store
  //store contains: title, body, and section
  store = indexJSON.store;

  //setup before functions
  var typingTimer;                //timer identifier
  var doneTypingInterval = 250;  //time in ms, 5 second for example

  //on keyup, start the countdown or check for enter
  searchBox.on('keyup', function (event) {
    //pressed the enter key, otherwise use a timer
    if(event.keyCode == 13){
      doneTyping (event)
    } else {
      clearTimeout(typingTimer);
      typingTimer = setTimeout(function() {doneTyping(event);}, doneTypingInterval);
    }
  });

  //on keydown, clear the countdown 
  searchBox.on('keydown', function () {
    clearTimeout(typingTimer);
  });

  //user is "finished typing," do something
  function doneTyping (event) {
    // Get query without white spaces
    var query = event.target.value.trim();

    // clear previous results
    resultdiv.hide();
    resultdiv.empty();

    //chekc for valid query
    if (query != "" && query.length > 1){
      //set result dir value for search item
      resultdiv.append('<li class="searchMatch">Matches for "' + query + '"</li>');

      // Search for it
      var result = index.search(query);

      // check if we found matches
      if (result.length === 0) {
        // nothing found
        resultdiv.append('<li class="searchItem">No results found</li>');
      } else {
        // display what we found
        for (var item in result) {
          var href = result[item].ref;
          var searchitem = '<li class="searchItem"><a class="searchLink" href="./../' + href + '" target="_blank" style="font-size:30px;">' + store[href].title + '</a><p class="location">' + store[href].section + '</p></li>';
          resultdiv.append(searchitem);
        }
      }
    };

    //show results
    resultdiv.show();
  }
});