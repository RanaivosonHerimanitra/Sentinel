
function RadioMeuble() {
    if ( true===true) {
        document.f.Algorithmes_eval[0].disabled = true;
        document.f.Algorithmes_eval[1].disabled = true;
        document.f.Algorithmes_eval[2].disabled = true;
    } 

}
$('#diseases').change(function(){
   alert($('#diseases').val() + ' was just selected');
});


//<input type="radio" name="radio1" value="L" id="radio1" onBlur="RadioMeuble()">á louer</label>
//<br>
//<label>