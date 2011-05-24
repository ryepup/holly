var holly = {};

holly.x10 = function(code, direction, callback){
  $.ajax({
	     url:'/api/x10',
	     data:{'code':code, 'dir':direction?'fon':'foff'},
	     success:function(){
		 if (callback) callback();
	     }
	 });  
};

holly.x10click = function(btn, code, direction){
    var keepGoing = true;
    var cb = function(){
	if (keepGoing){
	    $(btn).fadeToggle('slow', 'linear', cb);
	}else{
	    $(btn).fadeIn();
	}
	    
    };
    cb();
    holly.x10(code, direction, function(){
		  keepGoing = false;
	      });
};