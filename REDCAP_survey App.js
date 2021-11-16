//start code
g_survey_exceptions = [
    'baseline_mother_questionnaire', 
    'baseline_child_questionnaire', 
    'complementary_feeding_ffq', 
    'completion_questionnaire', 
    'individual_clinic_visit_record', 
    'monthly_questionnaire_8599', 
    'participant_registration_form', 
    'maternal_ffq', 
    'participant_weekly_record_old',
    'participant_medication_record_old',
    'participant_illness_record_old',
    'adverse_events',
    'protocol_deviation',
    ///////////repeating /////////
    //'participant_3_day_food_record',
    //'participant_5_day_sleep_diary',
    //'participant_actigraph_record',
    'participant_gp_visit_record',
    'participant_hospital_visit_record',
    'participant_intervention_adherence_record',
    'participant_illness_medication_record',
];

g_kickoff_surveyname = 'started_solids';  //once completed this survey makes the start time = now, i.e. day 0

g_html_styles = "<style>\nBODY{\nfont-family: OpenSans-Regular.ttf;\nfont-size: 7.3pt;\ncolor: #111111;\n}\n\nH3{\nfont-family: OpenSans-SemiBold.ttf;\nfont-size: 9pt;\nfont-weight: bold;\n}\n\nSTRONG{\nfont-family: OpenSans-SemiBold.ttf;\nfont-weight: bold;\n}\n</style>\n";

g_redcap_token = 'REDCAP TOKEN GOES HERE';
g_feedlist_comp=null;
g_submitform_comp=null;
g_newUserform_comp =null;
g_loginform_comp =null;
g_recordlist_comp = null;
g_current_question = -1;
g_current_surveyName = "";
g_current_eventName = "";
g_current_survey = null;
g_submitrecord_comp=null;
g_current_record_id = 0;
g_current_record = null;
g_current_events = null;
g_repeating_forms_events = null;
g_event_mappings = null;
g_instruments = null;
g_repeat_instance = -1;          
// g_survey_displayhistory = [];

g_answer_index = 0;
g_answers = [];

g_answered_surveys = [];  //if it is in the list we have submitted it, maybe diff rules for repeating surveys
//download stuff
g_download_survey_index = -1;
g_download_survey_total = 0;
g_download_survey_list = [];
g_download_progbar = null;
g_download_nextpage = '';
g_survey_output = [];

g_notification_title = 'Survey';
g_notification_body = 'Remember to give your baby the Kūmara powder today? Did your baby go to the doctor or have any medication today? Please record this on the app!';

//Other body messages event=>message
g_notification_other = {
    stool_2_arm_1: 'Start collecting your baby’s stool (poo) sample 32 hours after you started your 3-day food record!', 
    stool_3_arm_1: 'Start collecting your baby’s stool (poo) sample 32 hours after you started your 3-day food record!',
    sleep_diary_2_arm_1: 'Remember to complete your baby’s sleep diary on the app today!',
    sleep_diary_3_arm_1: 'Remember to complete your baby’s sleep diary on the app today!',
    food_record_1_arm_1: 'Remember to complete your baby’s food diary on the app today!',
    food_record_2_arm_1: 'Remember to complete your baby’s food diary on the app today!',
};

//custom action for login in App. Validating new user.
registerAction("redcap_newUser", "", "newUser form", "component:newUserform:form")  
   //the form component will get passed into my function
function redcap_newUser(form_comp) {
    g_newUserform_comp = form_comp;
    var jsonFormData = JSON.parse(gatherFormData(form_comp));
    var ID = jsonFormData.ID.toUpperCase().trim(); 
    print(' ID:' + ID);
    var pin = jsonFormData.pin.toLowerCase().trim();

    print('PIN:' + pin);
    var postbody = 'token=' + g_redcap_token + '&content=record&format=json&type=flat&fields=record_id,registration_pin,trigger_day0&filterLogic=' + urlEncode('[record_id] = "' + ID + '" and [registration_pin] = "' + pin + '"');    
    print(postbody)
    httpPost("link to REDCAP API GOES HERE", postbody, 'newUser_success', 'newUser_error');
}
   

function newUser_success(result){
    print('newUser_success=' + result);
    if (result == "[]"){
        //User not found
        print("User not found")
        U_call_method("component_event", global_script_object, g_newUserform_comp, 'on_send_error', '{"error": "Incorrect Participant ID"}');
    } else{
        //The user is found
        var jsonResult=JSON.parse(result)
        g_current_record_id =  jsonResult[0].record_id;
        print('g_current_record_id=' + g_current_record_id);
        setCookie("record_id", g_current_record_id);
        
        g_current_record = jsonResult;
        setCookie("user_record", result);
        print('ID:' + jsonResult[0].record_id)

        setCookie("redcap_answered_instruments", '[]'); //effectively set it to [], i.e. no answered surveys as far as the app knows
        
        //check if we have a value for 'trigger_day0', if so then the user has initialized on a different device and we should set this as the start unixtime in the cookie
        //and add notifications
        if (jsonResult[0].trigger_day0)
        print('trigger_day0=' , jsonResult[0].trigger_day0);

        check_for_start_date();

        
    }
}

function newUser_error(error_msg, status, result){
    if (result === null){
        result = '';
    }
    print('newUser_error (' + status + ')' + error_msg + ':' + result);
}

function check_for_start_date(){
    var postbody = 'token=' + g_redcap_token + '&content=record&format=json&type=flat&fields=trigger_day0&filterLogic=' + urlEncode('[record_id] = "' + g_current_record_id + '" and [trigger_day0] != ""');    
    print(postbody)
    httpPost("link to REDCAP API GOES HERE", postbody, 'checkStart_success', 'checkStart_error');
}

function checkStart_success(result){
    print('checkStart_success=' + result);
    if (result == "[]"){
        print("No start date found, probably users first time initializing");  //no problem it is probably their first time
    } else{
        //The start date is found
        var jsonResult=JSON.parse(result)
     
        //check if we have a value for 'trigger_day0', if so then the user has initialized on a different device and we should set this as the start unixtime in the cookie
        //and add notifications
        if (jsonResult[0].trigger_day0)
        print('trigger_day0=' , jsonResult[0].trigger_day0);

        //2020-09-04
        var startDate = new Date(jsonResult[0].trigger_day0);
        var unix_time = startDate.getTime() / 1000;
        setCookie('day_zero_timestamp', unix_time);
    }
    // go to welcome page
    U_call_method("component_event", global_script_object, g_newUserform_comp, 'on_send_success', '{}');
}

function checkStart_error(error_msg, status, result){
    if (result === null){
        result = '';
    }
    print('checkStart_error (' + status + ')' + error_msg + ':' + result);
}


 
 registerAction("redcap_disable_button_newuser", "", "Disable Button if new user has been initiated", "component:button:button")  
  
 function redcap_disable_button_newuser(button_comp) {
     //Is this survey active ?
     if (hasCookie("record_id")){
         //available so disable it
         callAction('set_button_enabled', button_comp, false);
     }else{
         //not available disable it
         callAction('set_button_enabled', button_comp, true);
     }
 }
 

// deleteCookie() delete the saved ID as an administrator.  So i can test that the ID is saving (i.e. re-enter as a new user).
registerAction("deleteInformation", '', "Delete Saved Info", "component:username:text_entry");

function deleteInformation(username){
    var user = getProperty(username,"text");
    deleteCookie(user);
 }


 // Login button disabled until new user initiates the app.
registerAction("redcap_disable_button_login", "", "Disable Button if new user not initiated", "component:button:button")  
  
function redcap_disable_button_login(button_comp) {
    //Is this survey active ?
    if (hasCookie("record_id")){
        //available so enable it
        callAction('set_button_enabled', button_comp, true);
    }else{
        //not available disable it
        callAction('set_button_enabled', button_comp, false);
    }
}

// make custom action for standard login that checks against saved ID   
registerAction("redcap_login", "", "login form", "component:form:form")  
   //the form component will get passed into my function
function redcap_login(form_comp) {
    var jsonFormData = JSON.parse(gatherFormData(form_comp));
    print(jsonFormData)
    var ID = jsonFormData.ID.toUpperCase().trim()                                        ; 
    print('ID:' + ID);
    var saved_id = getCookie("record_id");
    if (saved_id == ID){
        // go to welcome page
        print(saved_id)
        load_globals();
        U_call_method("component_event", global_script_object, form_comp, 'on_send_success', '{}');
        //g_current_record = getCookie("user_record");
    } else {
        //did not match show a popup
        print("User not found")
        U_call_method("component_event", global_script_object, form_comp, 'on_send_error', '{"error": "Incorrect Participant ID"}');
        
    }
}



//  start the survey in the app, but look up the right event for it, assume the survey is active
registerAction("redcap_startSurvey_form", "", "Start Survey Form", "string:SurveyName")  

function redcap_startSurvey_form(surveyName){
    print('Start survey: ' + surveyName);
    //get the list of active surveys
    var active_surveys = get_active_surveys();
    for (var prop in active_surveys){
        var temp_survey_name = active_surveys[prop]['form'];
        if (temp_survey_name == surveyName){
            //found it, get the unique_event_name
            eventName = active_surveys[prop]['unique_event_name'];
            redcap_startSurvey(surveyName, eventName);
            return;
        }
    }
    //hmm  did not find it
    print('Could not start ' + surveyName + ' cannot find the event in the active survey list ' , active_surveys);
}


//  start the survey in the app
registerAction("redcap_startSurvey", "", "Start Survey", "string:SurveyName,string:Event Name")  

function redcap_startSurvey(surveyName, eventName){
    print('Start survey: ' + surveyName + ' event=' + eventName);
    setGlobalVar('today_YYYYMMDD', formatDate(Date()));
    setGlobalVar('time_HHMM', formatTime(Date())); 
    print(formatTime(Date()))
    g_current_surveyName = surveyName;
    g_current_eventName = eventName;
    g_current_survey = null;
    g_current_question = -1; 
    var saved_surveyData = getCookie(g_current_surveyName);
    if (saved_surveyData == ''){
        //go get survey
        redcap_survey(g_current_surveyName);
    }else{
        g_current_survey = JSON.parse(saved_surveyData);
        redcap_showSurvey();
    }
}

//custom action for survey in App, that gets saved survey and puts questions into pages 
registerAction("redcap_showSurvey", "", "Show Survey", "")  
  
function redcap_showSurvey() {
    g_current_question = g_current_question + 1;
    print('survey.length=' +  g_current_survey.length)
    if (g_current_question >= g_current_survey.length){
        // have run out of questions
        callAction("show_page_by_name","Survey End",3); 
        //print('end survey');
    } else {
        var Question = g_current_survey[g_current_question];
        print('Question ' + g_current_question + ' field type=' + Question.field_type + ' field_name=' + Question.field_name + ' branching_logic='+Question.branching_logic);

 
        
        if (Question.branching_logic == '' ){
            //no branching logic, then show it
        }else{
            //print('there seems to be branching logic');
            if (branching_logic(Question.branching_logic)){
                //show it
            }else{
                //skip it, use delayCall so we don;t run out of stack space due to deep recursion
                delayCall('redcap_showSurvey', 1);
                return;
            }
        }

        //print('before fieldtype');
        switch(Question.field_type) {
            case 'descriptive': 
                if (Question.section_header == ""){
                    //skip it
                    delayCall('redcap_showSurvey', 1);
                }else{      
                    callAction("show_page_by_name","descriptive",3);
                }
              break;
            case 'radio':
                callAction("show_page_by_name","radio",3);
              break;
            case 'dropdown':
                callAction("show_page_by_name","dropdown",3);
                break;
            case 'yesno':
                callAction("show_page_by_name","yesno",3);
                break;
            case 'notes':
                if(Question.field_annotation.includes('@HIDDEN')){ 
                    delayCall('redcap_showSurvey', 1);    //case 'text':  // skip text questions
                }else{     
                    callAction("show_page_by_name","notes",3);
                }
                break;
                case 'file':
                callAction("show_page_by_name","photo",3);
                break;  
            case 'text':
                if(Question.field_annotation.includes('@HIDDEN')){  
                //if(Question.field_label.includes('>Interviewer<') || /*Question.field_label.includes('>Data checked for completeness by:<') ||*/Question.field_label.includes('>Date:<') || Question.field_label.includes('>Notes (e.g. if not collected or protocol violation)<') || Question.field_label.includes('>Date checked:<') ||  Question.field_label.includes('© 2008 - 2016 PROMIS Health Organisation and PROMIS Cooperative Group') ){
                    delayCall('redcap_showSurvey', 1);    //case 'text':  // skip text questions
                }else{ 
                    switch(Question.text_validation_type_or_show_slider_number){
						case 'time':
							callAction("show_page_by_name","time",3);
							break;	
						case 'date_dmy':
							callAction("show_page_by_name","date",3);
							break;
						case '':
							callAction("show_page_by_name","text",3);
							break;
						default:
							print('I do not know what the text_validation_type_or_show_slider_number is.' + Question.text_validation_type_or_show_slider_number );	
                    }
                }
                break;
            default:
            print('I do not know what the field type is.' + Question.field_type );
        }
        //print('after fieldtype');  
    }
    
}    

//custom action for submitting field data in a form into REDCAP
registerAction("redcap_start_submitSurvey", "", "submit Survey", "")  
//the form component will get passed into my function
function redcap_start_submitSurvey() {
    if (g_current_record == 0){
        //that is a problem, user is not logged in
        print("User is not logged in");
        print(g_current_record);
    }else{
        var surveyAnswers_str = getCookie(g_current_surveyName + '_answer');  
        if (is_form_repeating(g_current_surveyName)){
            g_repeat_instance = get_repeat_instance_num(g_current_surveyName);
            g_repeat_instance++;
            set_repeat_instance_num(g_current_surveyName, g_repeat_instance);
        }
        g_answers = JSON.parse(surveyAnswers_str);
        g_answer_index = -1;
        processAnswer();
    }
}


function redcap_start_RESEND_submitSurvey() {
    if (g_current_record == 0){
        //that is a problem, user is not logged in
        print("User is not logged in");
        print(g_current_record);
    }else{
        //All the globals like     g_repeat_instance    , g_current_surveyName and g_current_eventName are set
        g_answer_index = -1;
        processAnswer();
    }
}

function processAnswer(){
    //Loop through the questions and any file type ones we should submit as import file
    g_answer_index++;
    if (g_answer_index >= g_current_survey.length){
        //time to stop
        redcap_finish_submitSurvey();
    }else{

        var fieldname = g_current_survey[g_answer_index]['field_name'];
        var fieldType = g_current_survey[g_answer_index]['field_type']; //eg descriptive, file, text, radio
        var logic_string = g_current_survey[g_answer_index]['branching_logic']
        var answerValue = g_answers[fieldname];

        var fieldannotation = g_current_survey[g_answer_index]['field_annotation'];
        if (fieldannotation == 'history'){
            //get human readable answer value
            var human_answer = answerValue;
            //unless it is a radio then we need to get the human readable value
            if (fieldType == 'radio' || fieldType == 'dropdown'){
                var Question = g_current_survey[g_answer_index];
                
                var field_list =  Question.select_choices_or_calculations.split("|");
                var human_answer = field_list[answerValue].trim().split(',')[1].trim();
            }
            save_history(g_current_eventName, g_current_surveyName, fieldname, human_answer);
        }
        

        print('fieldname=' + fieldname + ' answerValue=' + answerValue);
       
        if (fieldType == 'file'){
            //check if we have an anser for this question and skip it if we have not answered
            if (answerValue !== undefined){
                if (logic_string == '' || branching_logic(logic_string)){
                    redcap_submitPhoto(answerValue, fieldname, g_repeat_instance, g_current_eventName);
                }else{
                    //skip it, due to logic
                    print('Skip photo ' + fieldname + ' due to branching logic');
                    delayCall('processAnswer', 1);    
                }
            }else{
                print('Skipping file as it was proibably skipped ' + fieldname);
                delayCall('processAnswer', 1);
            }
            //maybe remove it from the answers, otherwise the eav will need to know about it
        }else{
            //carry on to next question
            delayCall('processAnswer', 1);
        }
    }
} 


function redcap_finish_submitSurvey(){
    var output = [];
    for (var question_index in g_current_survey){
        var fieldname = g_current_survey[question_index]['field_name'];
        if (g_answers.hasOwnProperty(fieldname)){
            var fieldType = g_current_survey[question_index]['field_type']; //eg descriptive, file, text, radio
            var answerValue = g_answers[fieldname];
            //if(is_form_repeating == true){ 
                //var displayAns = g_survey_displayhistory;   
                //print(displayAns)
                if (fieldType == 'file'){
                    //skip it
                }else{
                    var repeat_instrument_name = '';
                    var repeat_instance_num = '';
                    var logic_string = g_current_survey[question_index]['branching_logic'];
                    if (logic_string == '' || branching_logic(logic_string)){
                        if (g_repeat_instance > 0){
                            repeat_instrument_name = g_current_surveyName;
                            repeat_instance_num = g_repeat_instance;
                        }
                        var newObj = convert_answer_to_eav(fieldname, answerValue, repeat_instrument_name, repeat_instance_num);
                        output.push(newObj); 
                    }else{
                        //  
                        print("skipping due to branching logic")
                    }   
                }
            //}
        }else{
            //skip
            print('Skipped answering ' + fieldname);
        }
    }

    print(output);

    var json_resultsString =  JSON.stringify(output);
    print(json_resultsString)
    var postbody = 'token=' + g_redcap_token + '&content=record&format=json&type=eav&returnContent=count[default]&overwriteBehavior=normal&forceAutoNumber=false&returnFormat=json&data=' + urlEncode(json_resultsString)    
    print(postbody)
    httpPost("link to REDCAP API GOES HERE", postbody, 'submit_survey_success', 'submit_survey_error');
}

function convert_answer_to_eav(fieldname, value, repeat_instrument_name, repeat_instance_num){
//function convert_answer_to_eav(answerObj, repeat_instrument_name, repeat_instance_num){
    print('convert_answer_to_eav');


    print('fieldname=' + fieldname + ' value=' + value);
    var newObj = {}
    newObj['record'] = g_current_record_id;
    newObj['field_name'] = fieldname;
    newObj['value'] = value;
    newObj['redcap_event_name'] = g_current_eventName;
    newObj['redcap_repeat_instrument'] = repeat_instrument_name;
    newObj['redcap_repeat_instance'] = repeat_instance_num;

    return newObj;
}
 

function submit_survey_success(result){
    print('submit_survey_success=' + result);
    //check if this survey was the kickoff survey g_kickoff_surveyname
    if (g_current_surveyName == g_kickoff_surveyname){
        //save the start_time
        set_day_zero();
    }
    g_answered_surveys.push(g_current_surveyName + '|' + g_current_eventName);  //combine the survey and event names, to handle a survey that should be filled in over multiple events but is not "repeating" in that event
    setCookie('redcap_answered_instruments', JSON.stringify(g_answered_surveys));

    if (count_failed_submits() > 0){
        check_for_failed_submits();  //try to resend another oneq
    }else{
        callAction("show_page_by_name", 'Main Menu',3); 
        deleteCookie(g_current_surveyName + '_answer');   // delete previous answers so they don't reappear in next repeated survey if branching logic appears 
    }
}

function set_day_zero(){
    var d = new Date();
    var unix_time = d.getTime() / 1000;
    print('now=' , unix_time);  //JS returns milliseconds, so div by 1000 to get seconds
    setCookie('day_zero_timestamp', unix_time);
    cancelAllNotifications();  //clear out any existing notifications
    delayCall('setup_local_notifications', 1);  //delay just in case the cancel all notifications takes a bit
}

function setup_local_notifications(){
    var now_offset = get_days_passed();
    print('setup_local_notifications now_offset=' + now_offset);
    //find current_day_offset, as this function might be called when user 
    //initializes a new phone after starting the surbey on another phone
    
    //Go through the events with day_offset > 0 and
    if (now_offset < 0){
        print('we have not started yet');
        //No notifications
    }else{
        print('we have started');
        //else yes we have started
        //convert now to a now_offset from, start_time
        
        //loop through events
        for (var prop in g_current_events){
            print('event=' + g_current_events[prop]['unique_event_name'] + ' day=' + g_current_events[prop]['day_offset']);
            var day_offset = parseInt(g_current_events[prop]['day_offset']);
            //check if event is in future
            if(day_offset >= now_offset){
                var in_days_time = day_offset - now_offset;
                var someDate = new Date();
                someDate.setDate(someDate.getDate() + in_days_time); 

                var notifydate = new Date(someDate.getFullYear(),someDate.getMonth(),someDate.getDate(),16,00,00);  //4pm aka 1600
                var unix_time = parseInt((notifydate.getTime() / 1000)); // not doing the offset, might be a bug in umajin but does not seem to be needed - (notifydate.getTimezoneOffset() * 60));
                var id = g_current_events[prop]['unique_event_name'];
                var body = g_notification_body;  //default body
                /*
                if (g_notification_other[id] !== undefined){
                    //looks like we want a special body for this event
                    //body = g_notification_other[id];
                }*/
                if (g_notification_other[id] == undefined){
                    print('g_notification_other[id] == undefined: ' + g_notification_other[id])
                    //cant find it, not in our body overwrite list
                }else{
                    //looks like we want a special body for this event
            
                    body = g_notification_other[id];
                }    
                print('notifyAtDateTime id=' + id + ' unix=' + unix_time + ' title=' + g_notification_title + ' body=' + body);
                //default message
                notifyAtDateTime(g_notification_title, body, id, unix_time);
            }
        }
    }
}

function submit_survey_error(error_msg, status, result){
    if (result === null){
        result = '';
    }
    print('submit_survey_error (' + status + ')' + error_msg + ':' + result);
    if (parseInt(status) == 0){
        //no result from server
        result = error_msg;
    }
    setGlobalVar('survey_error_code', status + ' : ' + result);
    redcap_error_submitSurvey(error_msg);
}

function redcap_error_submitSurvey(){
    //problem, maybe try again later???????
    //we save this info to a cookie
    var resend_list = [];
    if (hasCookie('survey_resend_list')){
        print('We have a saved list of fails ' + getCookie('survey_resend_list'));
        resend_list = JSON.parse(getCookie('survey_resend_list'));
        //get first item from list and pop it off the list, on fail should add it back again
        //info such as g_repeat_instance, g_current_surveyName, g_answers, g_current_eventName
    }else{
        print('First failed');
    }

    var item = {};
    item.repeat_instance = g_repeat_instance;
    item.surveyName = g_current_surveyName;
    item.answers = g_answers;
    item.eventName = g_current_eventName;
    resend_list.push(item);
    setCookie('survey_resend_list', JSON.stringify(resend_list));

    callAction("show_page_by_name", 'Submit Error',3); 
}


registerAction("check_for_failed_submits", "", "Check for Failed Submits", "");

function check_for_failed_submits(){
    if (hasCookie('survey_resend_list')){
        print('we have a survey_resend_list cookie');
        var resend_list = JSON.parse(getCookie('survey_resend_list'));
        print('survey_resend_list length=' + resend_list.length);
        if (resend_list.length > 0){
            //get first item from list and pop it off the list, on fail should add it back again
            //info such as g_repeat_instance, g_current_surveyName, g_answers, g_current_eventName
            item = resend_list.pop();
            var save_str = JSON.stringify(resend_list);
            print('save_str=' + save_str);
            setCookie('survey_resend_list', save_str); //save the shortened list back
            g_repeat_instance =  item.repeat_instance;
            g_current_surveyName = item.surveyName;
            g_answers =  item.answers;
            g_current_eventName = item.eventName;

            var saved_surveyData = getCookie(g_current_surveyName);
            g_current_survey = JSON.parse(saved_surveyData);
            
            setCookie(g_current_surveyName + '_answer', JSON.stringify(g_answers));
            
            print('RESEND event=' + g_current_eventName + ' survey=' + g_current_surveyName + ' repeat=' + g_repeat_instance + ' answers=' + g_answers);
            redcap_start_RESEND_submitSurvey();
        }else{
            //list is empty yay
        }
    }
}
  

// Fill description question from the survey
registerAction("display_count_failed_submits", "", "Count failed submits", "component:label:text")  
  
function display_count_failed_submits(text_comp) {
    var count = count_failed_submits();
    setProperty(text_comp, 'text', 'There are ' + count + ' surveys that need to be resent');
}


function count_failed_submits() {
    var count = 0;
    if (hasCookie('survey_resend_list')){
        var resend_list = JSON.parse(getCookie('survey_resend_list'));
        //get first item from list and pop it off the list, on fail should add it back again
        //info such as g_repeat_instance, g_current_surveyName, g_answers, g_current_eventName
        count = resend_list.length;
    }
    return count;
}




// Fill description question from the survey
registerAction("redcap_filldescription", "", "fill description in Survey", "component:filldescription:html_article")  
  
function redcap_filldescription(html_comp) {
    var saved_surveyData = getCookie(g_current_surveyName);
    var survey = JSON.parse(saved_surveyData);
    var Question = survey[g_current_question];
    print('field type=' + Question.field_type);
    
    //set HTML on htmp article
    setProperty(html_comp, 'text', g_html_styles + Question.section_header);
}

// Fill the radio question from the survey
registerAction("redcap_fillradio", "", "fill radio in Survey", "component:fillradio:html_article,component:but1:button,component:but2:button,component:but3:button,component:but4:button,component:but5:button,component:but6:button,component:but7:button,component:but8:button,component:history:button")  
  
function redcap_fillradio(html_comp,but1,but2,but3,but4,but5,but6,but7,but8, butHistory) {
    var saved_surveyData = getCookie(g_current_surveyName);
    var survey = JSON.parse(saved_surveyData);
    var Question = survey[g_current_question];
    print('field type=' + Question.field_type);
    
    //hide history button unless field_annotation == history
    if (Question.field_annotation == 'history'){
        //show button
        setProperty(butHistory, 'visible', 'True');
    }else{
        //hide button
        setProperty(butHistory, 'visible', 'False');
    }

    //set HTML on htmp article
    setProperty(html_comp, 'text', g_html_styles + Question.field_label);
    
    //"0, Very much | 1, Quite a bit | 2, Somewhat | 3, A little bit | 4, Not at all"
    // split string by "|" and "," then trim.
   
    var field_list =  Question.select_choices_or_calculations.split("|");
    print(field_list[0])
 
    if (field_list.length >= 1){
        var label1 = field_list[0].trim().split(',')[1].trim();
		setProperty(but1,'text', label1);
        setProperty(but1, 'visible','True')
	} else{
		setProperty(but1, 'visible','False')
    }

    if (field_list.length >= 2){
        var label2 = field_list[1].trim().split(',')[1].trim();
		setProperty(but2,'text', label2);
        setProperty(but2, 'visible','True')
	} else{
		setProperty(but2, 'visible','False')
    }

    if (field_list.length >= 3){
        var label3 = field_list[2].trim().split(',')[1].trim();
		setProperty(but3,'text', label3);
        setProperty(but3, 'visible','True')
	} else{
		setProperty(but3, 'visible','False')
    }

    if (field_list.length >= 4){
        var label4 = field_list[3].trim().split(',')[1].trim();
		setProperty(but4,'text', label4);
        setProperty(but4, 'visible','True')
	} else{
		setProperty(but4, 'visible','False')
    }

    if (field_list.length >= 5){
        var label5 = field_list[4].trim().split(',')[1].trim();
		setProperty(but5,'text', label5);
        setProperty(but5, 'visible','True')
	} else{
		setProperty(but5, 'visible','False')
    }

    if (field_list.length >= 6){
        var label6 = field_list[5].trim().split(',')[1].trim();
		setProperty(but6,'text', label6);
        setProperty(but6, 'visible','True')
	} else{
		setProperty(but6, 'visible','False')
    }

    if (field_list.length >= 7){
        var label7 = field_list[6].trim().split(',')[1].trim();
		setProperty(but7,'text', label7);
        setProperty(but7, 'visible','True')
	} else{
		setProperty(but7, 'visible','False')
    }

    if (field_list.length >= 8){
        var label8 = field_list[7].trim().split(',')[1].trim();
		setProperty(but8,'text', label8);
        setProperty(but8, 'visible','True')
	} else{
		setProperty(but8, 'visible','False')
    }
}

// Fill text question from the survey
registerAction("redcap_filltext", "", "fill text in Survey", "component:html:html_article,component:field_label:html_article,component:text:text_entry,component:history:button") 

function redcap_filltext(html_comp, html_comp_label, text_comp, butHistory) {
    var saved_surveyData = getCookie(g_current_surveyName);
    var survey = JSON.parse(saved_surveyData);
    var Question = survey[g_current_question];
    print('field type=' + Question.field_type);
    
    //hide history button unless field_annotation == history
    if (Question.field_annotation == 'history'){
        //show button
        setProperty(butHistory, 'visible', 'True');
    }else{
        //hide button
        setProperty(butHistory, 'visible', 'False');
    }

    //set HTML on htmp article
    setProperty(text_comp, 'text', '');
    setProperty(html_comp, 'text', g_html_styles + Question.section_header);
    setProperty(html_comp_label, 'text', g_html_styles + Question.field_label);
}

// Fill notes question from the survey
registerAction("redcap_fillnotes", "", "fill notes in Survey", "component:fillnotes:html_article,component:field_label:html_article,component:text:text_entry") 

function redcap_fillnotes(html_comp, html_comp_label, text_comp) { 
    var saved_surveyData = getCookie(g_current_surveyName);
    var survey = JSON.parse(saved_surveyData);
    var Question = survey[g_current_question];
    print('field type=' + Question.field_type);
    
    //set HTML on htmp article
    setProperty(text_comp, 'text', '');
    setProperty(html_comp, 'text', g_html_styles + Question.section_header);
    setProperty(html_comp_label, 'text', g_html_styles + Question.field_label);
}

//Fill dropdown question from the survey
registerAction("redcap_filldropdown", "", "fill dropdown in Survey", "component:html:html_article,component:filldropdown:dropdown_menu,component:but1:button,component:but2:button,component:but3:button,component:but4:button,component:but5:button,component:but6:button,component:but7:button,component:but8:button,component:but9:button,component:history:button")  
  
function redcap_filldropdown(html_comp,filldropdown,but1,but2,but3,but4,but5,but6,but7,but8,but9,butHistory) {
    var saved_surveyData = getCookie(g_current_surveyName);
    var survey = JSON.parse(saved_surveyData);
    var Question = survey[g_current_question];
    print('field type=' + Question.field_type);
    
    //hide history button unless field_annotation == history
    if (Question.field_annotation == 'history'){
        //show button
        setProperty(butHistory, 'visible', 'True');
        print('you should not see this')
    }else{
        //hide button
        setProperty(butHistory, 'visible', 'False');
    }

    //set HTML on htmp article
    setProperty(html_comp, 'text', g_html_styles + Question.field_label);
    
    //"0, Breakfast | 1, Morning Tea | 2, Lunch | 3, Afternoon Tea | 4, Dinner | 5, Supper | 6, Breastfeed | 7, Formula",
    // split string by "|" and "," then trim.
   
    var field_list =  Question.select_choices_or_calculations.split("|");
    print(field_list[0])
 
    if (field_list.length >= 1){
        var label1 = field_list[0].trim().split(',')[1].trim();
		setProperty(but1,'text', label1);
        setProperty(but1, 'visible','True')
	} else{
		setProperty(but1, 'visible','False')
    }

    if (field_list.length >= 2){
        var label2 = field_list[1].trim().split(',')[1].trim();
		setProperty(but2,'text', label2);
        setProperty(but2, 'visible','True')
	} else{
		setProperty(but2, 'visible','False')
    }

    if (field_list.length >= 3){
        var label3 = field_list[2].trim().split(',')[1].trim();
		setProperty(but3,'text', label3);
        setProperty(but3, 'visible','True')
	} else{
		setProperty(but3, 'visible','False')
    }

    if (field_list.length >= 4){
        var label4 = field_list[3].trim().split(',')[1].trim();
		setProperty(but4,'text', label4);
        setProperty(but4, 'visible','True')
	} else{
		setProperty(but4, 'visible','False')
    }

    if (field_list.length >= 5){
        var label5 = field_list[4].trim().split(',')[1].trim();
		setProperty(but5,'text', label5);
        setProperty(but5, 'visible','True')
	} else{
		setProperty(but5, 'visible','False')
    }

    if (field_list.length >= 6){
        var label6 = field_list[5].trim().split(',')[1].trim();
		setProperty(but6,'text', label6);
        setProperty(but6, 'visible','True')
	} else{
		setProperty(but6, 'visible','False')
    }

    if (field_list.length >= 7){
        var label7 = field_list[6].trim().split(',')[1].trim();
		setProperty(but7,'text', label7);
        setProperty(but7, 'visible','True')
	} else{
		setProperty(but7, 'visible','False')
    }

    if (field_list.length >= 8){
        var label8 = field_list[7].trim().split(',')[1].trim();
		setProperty(but8,'text', label8);
        setProperty(but8, 'visible','True')
	} else{
		setProperty(but8, 'visible','False')
    }

    if (field_list.length >= 9){
        var label9 = field_list[8].trim().split(',')[1].trim();
		setProperty(but9,'text', label9);
        setProperty(but9, 'visible','True')
	} else{
		setProperty(but9, 'visible','False')
    }
}

// Fill yesno question from the survey
registerAction("redcap_fillyesno", "", "fill yesno in Survey", "component:fillyesno:html_article") 

function redcap_fillyesno(html_comp) {
    var saved_surveyData = getCookie(g_current_surveyName);  
    var survey = JSON.parse(saved_surveyData);
    var Question = survey[g_current_question];
    print('field type=' + Question.field_type);
    
    //set HTML on htmp article
    setProperty(html_comp, 'text', g_html_styles + Question.field_label);
        
}

//custom action for taking a photo 
registerAction("setup_photo", "", "setup photo", "component:field_label:html_article")  
function setup_photo(html_comp_label){
    var saved_surveyData = getCookie(g_current_surveyName);  
    var survey = JSON.parse(saved_surveyData);
    var Question = survey[g_current_question];
    setProperty(html_comp_label, 'text', g_html_styles + Question.field_label);
}

//custom action for taking a photo 
registerAction("take_photo", "", "take photo", "")  
function take_photo(){
    cameraCapture('captureResult');
} 

function captureResult(photo) {
	if (photo=='') {
		callAction('show_popup','Photo Cancelled!');  // this goes back a page so user can click no to skip photo page.
	} else {
        callAction('show_popup', 'Photo taken: ' + photo);
        redcap_setAnswer(photo);
    } 
}

//custom action for skipping photo 
registerAction("skip_photo", "", "skip photo", "")  
function skip_photo(){
    redcap_showSurvey();
}
//custom action for submitting photo/s to REDCAP
    
function redcap_submitPhoto(local_file_path, field_name, repeat_instance_num, event_name) {   
    print('local file path=' + local_file_path)
    var temp_path = getGlobal('temp_path');
    var filename = getFilename(local_file_path);
    var temp_filename = temp_path + '/' + filename;
    renameFile(local_file_path, temp_filename) ;

    print('temp filename=' + temp_filename);

    url = "link to REDCAP API GOES HERE";
    data = '';
    headers = '';
    var ref = httpNew();
    httpAddFormData(ref, 'token', g_redcap_token);
    httpAddFormData(ref, 'content', 'file');
    httpAddFormData(ref, 'action', 'import');
    httpAddFormData(ref, 'record', g_current_record_id);
    httpAddFormData(ref, 'event', event_name);
    httpAddFormData(ref, 'field', field_name);
    httpAddFormData(ref, 'repeat_instance', repeat_instance_num);
    httpAddFormData(ref, 'returnFormat', 'json');
    httpAddFormFile(ref, 'file', temp_filename);
    httpPostFromRef(ref,url, data, headers, 'photo_success', 'photo_error');

}

function getFilename(filepath){
    return filepath.split('\\').pop().split('/').pop();
}
   

function photo_success(result){
    print('photo_success=' + result);
    processAnswer();
   
}

function photo_error(error_msg, status, result){
    if (result === null){
        result = '';
    }
    print('photo_error (' + status + ')' + error_msg + ':' + result);
    //submit answer error
   
    redcap_error_submitSurvey();
}

//true or false top whether this question with the branch logic should show
//excample : logic_string = "[bisq_role] = '3'"
function branching_logic(logic_string) {
    var parts =  logic_string.split(" ");
    print('logic_string=[' + logic_string + '] ' + g_current_question);
    //parts[0] = '[bisq_role]'
    //parts[1] = '='
    //parts[2] = "'3'"
    var branched_field_name = parts[0].replace('[', '').replace(']', '')
    var operator = parts[1]
    var branched_field_value = parts[2].replace(/'/g, '');  //note: gotta use regex tro replace more than just one occurrence
    
    
    var saved_surveyData = getCookie(g_current_surveyName + '_answer');
    if (saved_surveyData == ''){
        saved_surveyData = {};
    }else{
        saved_surveyData = JSON.parse(saved_surveyData);
    }


    switch(operator){
        case '=':
            //Look in our answers
            print('saved_surveyData[' + branched_field_name + ']=' + saved_surveyData[branched_field_name] + ' == ' + branched_field_value);
            return (saved_surveyData[branched_field_name] == branched_field_value);
            break;
        
        default:
            print('Unknown branching_logic operator ' + operator);
            return false;
        //add more if we need them
    }
}

//Associate value with numerical value
registerAction("redcap_setAnswer", "", "Set Answer", "string:Answer")  
  
function redcap_setAnswer(Answer) {
    var saved_surveyData = getCookie(g_current_surveyName);
    var survey = JSON.parse(saved_surveyData);
    var Question = survey[g_current_question];
    var field_name = Question.field_name;
    print('redcap_setAnswer field_name=' + Question.field_name + ' set answer=' + Answer);

    var saved_surveyData = getCookie(g_current_surveyName + '_answer');
    if (saved_surveyData == ''){
        saved_surveyData = {};
    }else{
        saved_surveyData = JSON.parse(saved_surveyData);
    }
    saved_surveyData[field_name]=Answer;
    print(saved_surveyData);
    setCookie(g_current_surveyName + '_answer', JSON.stringify(saved_surveyData));

    redcap_showSurvey()
}


registerAction("redcap_setAnswerText", "", "Set Answer Text", "component:Answer:text_entry")  
  
function redcap_setAnswerText(textObj) {
    var answer = getProperty(textObj, 'text');
    redcap_setAnswer(answer);
}


registerAction("redcap_clearCookies", "", "Clear Cookies", "string:key")  
  
function redcap_clearCookies(key) {
    deleteCookie(key);
}



// custom event feed
registerFeed('redcap_eventfeed', 'Event Feed', ''); //name of function, description of function

function redcap_eventfeed(component) {

  //Simply apply 'stringified' JSON data using the 'setCustomFeed' function
    if (g_current_events == null){
        
        returnvalue = getCookie('redcap_events');
        if (returnvalue == ""){
            returnvalue = '[{"event_name": "Enrollment ","arm_num": 1,"day_offset": 0,"offset_min": 0,"offset_max": 0,"unique_event_name": "enrollment_arm_1","custom_event_label": null}]';
        } 
        g_current_events = JSON.parse(returnvalue);
    }

    //figure out what events to show day_offset
    //WE WILL Have a cookie to store the start date
    // we will figure out how many days have passed
    // we will show all the events that are not complete.  day_offset < days_passed
    //seperate list of completed events stored in a cookie
    var days_passed = get_days_passed();
    var event_output = [];

    //loop through events
    for (var prop in g_current_events){
        print('event=' + g_current_events[prop]['event_name'] + ' day=' + g_current_events[prop]['day_offset']);
        if(parseInt(g_current_events[prop]['day_offset']) <= days_passed){
            event_output.push(g_current_events[prop]); 
        }
        
    }
    setCustomFeed(component, JSON.stringify(event_output));
}


function get_days_passed(){
    //WE WILL Have a cookie to store the start date
    var day_num = -1;   //default to -90
    if (hasCookie('day_zero_timestamp')){
        var start_unix_time = parseInt(getCookie('day_zero_timestamp'));
        var d = new Date();
        var now_unix_time =  d.getTime() / 1000;

        day_num = parseInt((now_unix_time - start_unix_time) / 86400);
    }
    // we will figure out how many days have passed
    //return -1 if we have not started yet
    //day_num = 7;  //week 1(kumara/health), sleep 1(actigraph/sleep diary), started solids:  testing delete this
    //day_num = 14;  //week 1(kumara/health),started solids:  testing delete this
    //day_num = 15;  //week 1(kumara/health):  testing delete this
    //day_num = 87;  //week 1(kumara/health), food record 1: testing delete this
    //day_num = 88;  //week 1(kumara/health), food record 1, stool record 2, breast milk 2: testing delete this
    //day_num = 90;  //week 1(kumara/health), food record 1, stool record 2, breast milk 2, sleep record 2(actigraph/sleep diary/2 x surveys/BISQ): testing delete this
    //day_num = 101;  //week 1(kumara/health), food record 1, stool record 2, breast milk 2, sleep record 2(actigraph/sleep diary/2 x surveys/BISQ): testing delete this
    //day_num = 102;  //week 1(kumara/health), stool record 2, breast milk 2, sleep record 2(actigraph/sleep diary/2 x surveys/BISQ): testing delete this
    //day_num = 103;  //week 1(kumara/health), sleep record 2(actigraph/sleep diary/2 x surveys/BISQ): testing delete this
    //day_num = 104;  //week 1(kumara/health), sleep record 2(actigraph/sleep diary/2 x surveys/BISQ): testing delete this
    //day_num = 105;  //week 1(kumara/health): testing delete this
    //day_num = 177; //week 1(kumara/health), food record 2: testing delete this
    //day_num = 178; //week 1(kumara/health), food record 2, stool record 3, breast milk 3: testing delete this
    //day_num = 180; //week 1(kumara/health), food record 2, stool record 3, breast milk 3, sleep record 3(actigraph/sleep diary/3 x surveys/BISQ): testing delete this
    //day_num = 191;  //week 1(kumara/health), food record 2, stool record 3, breast milk 3, sleep record 3(actigraph/sleep diary/3 x surveys/BISQ): testing delete this
    //day_num = 192;  //week 1(kumara/health), stool record 3, breast milk 3, sleep record 3(actigraph/sleep diary/3 x surveys/BISQ): testing delete this
    //day_num = 193;  //week 1(kumara/health): sleep record 3(actigraph/sleep diary/3 x surveys/BISQ): testing delete this
    //day_num = 194;  //week 1(kumara/health), sleep record 3(actigraph/sleep diary/3 x surveys/BISQ): testing delete this
    //day_num = 195;  //week 1(kumara/health): testing delete this
    print('get_days_passed=' , day_num);
    return day_num;
}

// custom survey feed, need to get the legit events and then show the surveys not in the exception list
registerFeed('redcap_surveyfeed', 'Survey Feed', ''); //name of function, description of function

function redcap_surveyfeed(component) {
    var survey_output = get_active_surveys();
    setCustomFeed(component, JSON.stringify(survey_output));
}

//Do the bulk of building up an active survey feed list, also used by is_survey_available function
function get_active_surveys(){
    var now_offset = get_days_passed();
    var event_output = [];
    var survey_output = [];

    //Have we reached day0 yet
    if (now_offset < 0){
        print('we have not started yet');
        //if no show all the events <= dayoffset 0
        for (var prop in g_current_events){
            //print('event=' + g_current_events[prop]['event_name'] + ' day=' + g_current_events[prop]['day_offset']);
            day_offset = parseInt(g_current_events[prop]['day_offset']);
            //show surveys for events  where  now_offset >= day_offset && now_offset <= day_offset +  6
            if(day_offset <= 0){
                event_output.push(g_current_events[prop]); 
            }
        }
    }else{
        print('we have started');
        //else yes we have started
        //convert now to a now_offset from, start_time
        
        //loop through events
        for (var prop in g_current_events){
            //print('event=' + g_current_events[prop]['event_name'] + ' day=' + g_current_events[prop]['day_offset']);
            day_offset = parseInt(g_current_events[prop]['day_offset']);
            //show surveys for events  where  now_offset >= day_offset && now_offset <= day_offset +  6

            if(now_offset >= day_offset && now_offset <= (day_offset +14)){
                event_output.push(g_current_events[prop]); 
            }
        }
    }
    
    //print('events=' , event_output);

    if (g_event_mappings == null){
        returnvalue = getCookie('redcap_eventMapping');
        if (returnvalue == ""){
            returnvalue = '[{"arm_num": 1,"unique_event_name": "enrollment_arm_1","form": "participant_registration_form", "label":"Participant Registration Form"}]';
        }
        g_event_mappings = JSON.parse(returnvalue);
    }

    //print('g_event_mappings=' , g_event_mappings);

    //NBow loop through the valid events, and find surveys
    for (var prop in event_output){  //prop is an int
        var current_event = event_output[prop]['unique_event_name'];
        //print('check event ' + current_event + ' for valid surveys');
       
        //get a list of survey's that match the event name
        for (var prop in g_event_mappings){
            //print('g_event_mappings event=' + g_event_mappings[prop]['unique_event_name'] + ' == ' + current_event);
            if(g_event_mappings[prop]['unique_event_name'] == current_event){
                //check if survey name is in the exception list
                var survey_name = g_event_mappings[prop]['form'];
                //print('check array ' + survey_name);
                if (check_array(g_survey_exceptions, survey_name)){
                    //dont include it
                    //print(survey_name + ' is in the exption list');
                }else{
                    //find the Nice name for the survey
                    //print('add ' + survey_name + ' to the feed');
                    //check if it has been answered and is not a repeating instrument
                    if (is_survey_answered(survey_name, current_event) && !is_form_repeating(survey_name)){
                        //hide it
                    }else{
                        label = get_instrument_label(survey_name);
                        g_event_mappings[prop]['label'] = label; //todo maybe do this at the download survey data bit at the start
                        survey_output.push(g_event_mappings[prop]); 
                    }
                }
            }
        }
    }
   return survey_output;
}

//Go get the eventInstrumentMapping, get the events and get the instruments (i.e. metadata)
registerAction("start_download", "", "Start Download", "string:next_page,component:progress:custom");

function start_download(next_page, progbar){
    g_download_nextpage = next_page;
    g_download_progbar = progbar;

    if (hasCookie('redcap_eventMapping')){
        //ok we already have stuff, skip on
        //load some globals
        load_globals();
        finish_download(false);
    }else{
        print('Download all the data');
        setGlobal('http_spinner', 'False');
        progressBar_update(g_download_progbar, 0, 100);  //set progress to 0
        redcap_instruments();//go get the eventmapping that will take progress to 5%, getting the events will take it to 10%, then the surveys are the rest
    }
}


//get the survey real names etc

function redcap_instruments(){
    httpPost("link to REDCAP API GOES HERE", 'token=' + g_redcap_token + '&content=instrument&format=json&returnFormat=json', "instrument_success", "instrument_failure");
    print('get Instruments');
} 

function instrument_success(returnvalue, responseHeaders, cookieJar ){
    //print('Get Event Mapping success: ' + returnvalue);
    //Check first char is '['
    setCookie('redcap_instruments', returnvalue);  //put it in a cookie
    g_instruments = JSON.parse(returnvalue);     //put it in a global
    progressBar_update(g_download_progbar, 5, 100);  //set progress to 5%

    for(var prop in g_instruments){
        g_download_survey_list.push(g_instruments[prop]['instrument_name']);  //the pretty name is in instrument_label
    }

    g_download_survey_total = g_instruments.length;
    print('g_download_survey_total=' + g_download_survey_total);
    redcap_eventMapping(); //now go get the events
}

function instrument_failure(errormessage, httpStatusCode, responseBody, responseHeaders, cookieJar) {
    print("failed to retrieve the instruments");
}



//custom action: get Instrument Event Mapping from REDCAP and store in a Cookie.

function redcap_eventMapping() {
    httpPost("link to REDCAP API GOES HERE", 'token=' + g_redcap_token + '&content=formEventMapping&format=json&returnFormat=json', "eventMapping_success", "eventMapping_failure");
    print('get Event Mapping')
} 

function eventMapping_success(returnvalue, responseHeaders, cookieJar ){
    //print('Get Event Mapping success: ' + returnvalue);
    //Check first char is '['
    setCookie('redcap_eventMapping', returnvalue);  //put it in a cookie
    g_event_mappings = JSON.parse(returnvalue);     //put it in a global
    progressBar_update(g_download_progbar, 10, 100);  //set progress to 5%

    redcap_repeating_instruments(); //now go get the events
}

function eventMapping_failure(errormessage, httpStatusCode, responseBody, responseHeaders, cookieJar) {
    print("failed to retrieve the Event Mapping");
}


//custom action: get Instrument Event Mapping from REDCAP and store in a Cookie.

function redcap_repeating_instruments() {
    httpPost("link to REDCAP API GOES HERE", 'token=' + g_redcap_token + '&content=repeatingFormsEvents&format=json&returnFormat=json', "repeating_instruments_success", "repeating_instruments_failure");
    print('get Repeating Forms and Events')
} 

function repeating_instruments_success(returnvalue, responseHeaders, cookieJar ){
    print('repeating_instruments_success: ' + returnvalue);
    //Check first char is '['
    //[{"event_name":"", "form_name":"", "custom_form_label":""}, ... ]
    setCookie('redcap_repeatingFormsEvents', returnvalue);  //put it in a cookie
    g_repeating_forms_events = JSON.parse(returnvalue);     //put it in a global

    progressBar_update(g_download_progbar, 15, 100);  //set progress to 5%

    redcap_events(); //now go get the events
}

function repeating_instruments_failure(errormessage, httpStatusCode, responseBody, responseHeaders, cookieJar) {
    print("failed to retrieve the Repeating Forms and Events");
}


//custom action: get Events from REDCAP and store in a Cookie.

function redcap_events() {
    httpPost("link to REDCAP API GOES HERE", 'token=' + g_redcap_token + '&content=event&format=json&returnFormat=json', "events_success", "events_failure");
    print('get events')
} 

function events_success(returnvalue, responseHeaders, cookieJar ){
    //print('Get events success: ' + returnvalue);
    //Check first char is '['
    setCookie('redcap_events', returnvalue);
    g_current_events = JSON.parse(returnvalue);

    progressBar_update(g_download_progbar, 20, 100);  //set progress to 10%
    //Start the survey metadata download
    redcap_survey();
}

function events_failure(errormessage, httpStatusCode, responseBody, responseHeaders, cookieJar) {
    print("failed to retrieve the events");
}

//custom action: get metadata from REDCAP and store in a Cookie.  

function redcap_survey() {
    g_download_survey_index++;
    if (g_download_survey_index >= g_download_survey_total){
        //time to stop, we have them all
        finish_download(true);
    }else{
        var surveyName = g_download_survey_list[g_download_survey_index];
        g_current_surveyName = surveyName;
        httpPost("link to REDCAP API GOES HERE", 'token=' + g_redcap_token + '&content=metadata&format=json&returnFormat=json&forms[0]=' + urlEncode(surveyName), "survey_success", "survey_failure");
        print('get survey=' + surveyName);
    }
} 

function survey_success(returnvalue, responseHeaders, cookieJar ){
    //print('Get survey success: ' + g_current_surveyName + ' ' + returnvalue);
    //TODO:Check the result looks like valid json and not rando html from a captive portal
    //Check first char is '['
    setCookie(g_current_surveyName, returnvalue);
    //g_current_survey = JSON.parse(returnvalue);
    //redcap_showSurvey();
    
    //if form is in g_repeating_forms_events then get the repeat_instance number
    if (is_form_repeating(g_current_surveyName)){
        //get first fieldname
        var temp = JSON.parse(returnvalue);
        //NEED TO FIND FIRST QUESTION THAT IS ACTUALLY A QUESTION AND NOT A DESCRIPTION !!!!!!!!!!
        for (var prop in temp){
            if (temp[prop]['field_type'] == 'descriptive'){
                //skip it
            }else{
                var fieldname = temp[prop]['field_name'];
                print('first field=' + fieldname);
                redcap_repeat_instance_number(fieldname);
                break;
            }
        }
    }else{
        //carry on to next form
        progressBar_update(g_download_progbar, 20 + ((g_download_survey_index / g_download_survey_total) * 80), 100);  //set progress to 10%
        redcap_survey();
    }
}

function survey_failure(errormessage, httpStatusCode, responseBody, responseHeaders, cookieJar) {
    print("failed to retrieve the survey");
}

//All done
function finish_download(initialSetup){
    print('finish download all the stuff');
    if(initialSetup){
        setup_local_notifications();  //just in case we have started on a diff phone
    }
    
    setGlobal('http_spinner', 'True');
    callAction("show_page_by_name",g_download_nextpage,3); 
}


//Use the g_survey_name var
//Find the first field
//Export record for that field, look thro0ugh the results to get the largest "redcap_repeat_instance" value
//Store the value in a cookie
function redcap_repeat_instance_number(fieldName) {
    print('redcap_repeat_instance_number=' + fieldName);
    //find the first field in the form
    var postbody = 'token=' + g_redcap_token + '&content=record&type=eav&format=json&filterLogic=' + urlEncode('[record_id]=' + g_current_record_id) + '&fields[0]=' + urlEncode(fieldName);
    print('postbopdy=' + postbody);
    httpPost("link to REDCAP API GOES HERE", postbody, "repeat_num_success", "repeat_num_failure");
} 

function repeat_num_success(returnvalue, responseHeaders, cookieJar ){
    print('Get repeat num success: ' + g_current_surveyName + ' ' + returnvalue);
    //Check first char is '['
    //find loargest value of "redcap_repeat_instance"
    var results = JSON.parse(returnvalue);
    var largest_num = 0;
    for(var prop in results){
        current_num = parseInt(results[prop].redcap_repeat_instance);
        if (current_num > largest_num){
            largest_num = current_num;
        }
    }
    
    set_repeat_instance_num(g_current_surveyName,largest_num);
    //g_current_survey = JSON.parse(returnvalue);
    //redcap_showSurvey();
    progressBar_update(g_download_progbar, 20 + ((g_download_survey_index / g_download_survey_total) * 80), 100);  //set progress to 10%
    redcap_survey();
}

function repeat_num_failure(errormessage, httpStatusCode, responseBody, responseHeaders, cookieJar) {
    print("failed to retrieve the repeat num");
}





function load_globals(){
    g_instruments = JSON.parse(getCookie('redcap_instruments'));
    g_current_events = JSON.parse(getCookie('redcap_events'));
    g_event_mappings = JSON.parse(getCookie('redcap_eventMapping'));
    g_repeating_forms_events = JSON.parse(getCookie('redcap_repeatingFormsEvents'));
    g_current_record = getCookie("user_record");
    g_current_record_id = getCookie("record_id");
    if (hasCookie('redcap_answered_instruments')){
        g_answered_surveys = JSON.parse(getCookie("redcap_answered_instruments"));
    }
}


/*
//This does not seem to work
TypeError: undefined not callable (property 'includes' of [object Array])
            at (REDCAP_HVN.js:601)
var fruits = ["Banana", "Orange", "Apple", "Mango"];
var n = fruits.includes("Mango");
print('n=' , n);
*/

//return true/false, as includes does not seem to work
function check_array(myarray, value){
    for (var prop in myarray){
        //print('check_array ' + prop + ' == ' + value);
        if (myarray[prop] == value){
            return true;
        }
    }
    return false;
}

function get_instrument_label(instrument_name){
    if (g_instruments == null){
        //problem
    }else{
        for(var prop in g_instruments){
            if (g_instruments[prop]['instrument_name'] == instrument_name){
                return g_instruments[prop]['instrument_label'];
            }
        }
    }
    return instrument_name;  //couldn't find it that is weird, maybe return the instrument name instead
}

function is_form_repeating(formName){
    //[{"event_name":"", "form_name":"", "custom_form_label":""}, 
    print('is_form_repeating ' + formName);
    for(var prop in g_repeating_forms_events){
        print('check=' + g_repeating_forms_events[prop].form_name + ' == ' + formName);
        if (g_repeating_forms_events[prop].form_name == formName){
            return true;
        }
    }
    return false;
}

function get_repeat_instance_num(formName){
    return getCookie(formName + '_repeat_instance');
}

function set_repeat_instance_num(formName, newValue){
    print('set_repeat_instance_num formName = ' + formName + ' newValue = ' + newValue) 
    return setCookie(formName + '_repeat_instance', newValue);
}


function formatDate(date) {
    var d = new Date(date),
    month = '' + (d.getMonth() + 1),
    day = '' + d.getDate(),
    year = d.getFullYear();

    if (month.length < 2) 
        month = '0' + month;
    if (day.length < 2) 
        day = '0' + day;

    return [year, month, day].join('-');
}

function formatTime(time) {
    var t = new Date(time),
    minute = '' + t.getMinutes(),
    hour = '' + t.getHours();

    if (minute.length < 2) 
        minute = '0' + minute;
    if (hour.length < 2) 
        hour = '0' + hour;

    return [hour, minute].join(':');
}

 //Go get the eventInstrumentMapping, get the events and get the instruments (i.e. metadata)
registerAction("survey_backbutton", "", "Go back a page", "");

function survey_backbutton(){
    print('survey_backbutton');
    g_current_question = g_current_question - 2;  // g_current_question --; is the shortcut for this line
    redcap_showSurvey();
    //callAction("navigate_back");
}

 
// custom action 
registerAction("date_format", "", "Format the Date", "component:dateFormat:text_entry,component:but:button")

function date_format(text_comp,button_comp){
    //date_format('20~!@#$%^&*()_+|\=-}{[],.":<>.,/?sd 20-08-01');
    var format = getProperty(text_comp, 'text')
    //var format = text_comp;
    var output = '';
    format = format.replace(/-/g, '')

    for(var i in format){
        char = format[i];
        print(char);
        if (isDigit(format[i])){
            //good
            output += char;
        }
        //increment space if index == 4 place '-'
        if(output.length == 4){   
            output += '-'; 
        }
        //increment space if index == 6 place '-'
        if (output.length == 7){
            output += '-';
        }
        if (output.length >= 10){
            print('output=[' + output + ']');
            setProperty(text_comp, 'text', output);
            print('button is enabled ' + output.length);
            callAction('set_button_enabled', button_comp, true);
            return;
        }
    }
    //if we get here then it is not right
    callAction('set_button_enabled', button_comp, false);
    print('button is disabled ' + output.length);
}

function isDigit(n) {
    return Boolean([true, true, true, true, true, true, true, true, true, true][n]);
}

// custom action 
registerAction("time_format", "", "Format the Time", "component:TimeFormat:text_entry,component:but:button")

function time_format(text_comp,button_comp){
    var format = getProperty(text_comp, 'text')
    //var format = text_comp;
    var output = '';
    format = format.replace(/-/g, '')

    for(var i in format){
        char = format[i];
        print(char);
        if (isDigit(format[i])){
            //good
            output += char;
        }
        //increment space if index == 4 place '-'
        if(output.length == 2){   
            output += ':'; 
        }
        if (output.length >= 5){
            print('output=[' + output + ']');
            setProperty(text_comp, 'text', output);
            print('button is enabled ' + output.length);
            callAction('set_button_enabled', button_comp, true);
            return;
        }
    }
    //if we get here then it is not right
    callAction('set_button_enabled', button_comp, false);
    print('button is disabled ' + output.length);
    
}


function is_survey_available(survey_name){
    //get a list of active surveys
    var active_surveys = get_active_surveys();
    //loop through them and see if the one we want is in there
    for (var prop in active_surveys){
        var temp_survey_name = active_surveys[prop]['form'];
        if (temp_survey_name == survey_name){
            return true;
        }
    }
    //never found it
    return false;
}


// Fill description question from the survey
registerAction("redcap_disable_button_survey", "", "Disable Button if Survey inactive", "string:survey_name,component:button:button")  
  
function redcap_disable_button_survey(survey_name, button_comp) {
    //Is this survey active ?
    if (is_survey_available(survey_name)){
        //available so enable it
        callAction('set_button_enabled', button_comp, true);
    }else{
        //not available disable it
        callAction('set_button_enabled', button_comp, false);
    }
}


//Look through the answereds surveys
function is_survey_answered(surveyName, eventName){
   return check_array(g_answered_surveys, surveyName+'|' + eventName);
}


function save_history(eventname, surveyname, field_name, current_answer){
    print('save_history start');
    //get history from cookie, push onto top of list, pop off the end, latest always first
    //save back into cookie
    //need to know the value and the datetime
    var mydate = new Date()
    date = mydate.getDate()
    month = mydate.getMonth()
    day = mydate.getDay()
    var months = [
    'Jan',
    'Feb',
    'March',
    'April',
    'May',
    'June',
    'July',
    'Aug',
    'Sept',
    'Oct',
    'Nov',
    'Dec'
    ]


    var monthName = months[month]
    console.log(monthName) // January

    var days = [
    'Sunday',
    'Monday',
    'Tueday',
    'Wednesday',
    'Thursday',
    'Friday',
    'Saturday'
    ]

    var dayName = days[day] // Thu
    var formatted = monthName + ' ' + date + ', ' + dayName;

    console.log(formatted) // Thu, 23 January 2019
    //var current_unix = parseInt(mydate.getTime() / 1000);
    var key = 'history_' + eventname + '_' + surveyname + '_' + field_name;    
    var history = [];
    if (hasCookie(key)){
        history = JSON.parse(getCookie(key));
    }
    var item = {datetime: 'Submitted: ' + formatted, answer: 'Answer: ' + current_answer};
    //var item = {datetime: current_unix, answer: current_answer};
    history.push(item);
    if (history.length > 6){
        history.shift()  // shift takes it off the end of the array
    }
   

    //save it back to the cookie
    setCookie(key, JSON.stringify(history));
    print('save_history end');
}

registerFeed('redcap_historyfeed', 'History Feed', ''); //name of function, description of function
function redcap_historyfeed(component) {
    var history = [{datetime:1601076897, answer:4}];
    if (g_current_survey != null){
        history = get_history(g_current_eventName, g_current_surveyName, g_current_survey[g_current_question]['field_name']);
    }
    //maybe format the datetime?
    setCustomFeed(component, JSON.stringify(history));
}


function get_history(eventname, surveyname, field_name){
    //get from cookie, maybe format date nicely
    var key = 'history_' + eventname + '_' + surveyname + '_' + field_name;    
    //var history = [];
    var history = [];
    if (hasCookie(key)){
        history = JSON.parse(getCookie(key));
    }
    history = history.reverse();  // reverses the array
    return history;
}

/*
//testing date 
var mydate = new Date()
print('TZ offset=' + mydate.getTimezoneOffset());  //retuning -720 for NZT  , negative is wrong
mydate.setDate(mydate.getDate() + 5);
print('TZ offset=' + mydate.getTimezoneOffset());
print(mydate.getFullYear() + '-' + mydate.getMonth() + '-' + mydate.getDate() + 'T' + mydate.getHours() + ':' + mydate.getMinutes() + 'TZ' + mydate.getTimezoneOffset());
print('unixtime =' + parseInt((mydate.getTime() / 1000) + (mydate.getTimezoneOffset() * 60 * -1)));
print('get_time=' + parseInt(mydate.getTime() / 1000));  //without adjusting for TZ it seems to be unixtime, which could be ok

var notifydate = new Date(mydate.getFullYear(),mydate.getMonth(),mydate.getDate(),16,00,00);  //4pm aka 1600
print('notify date=' + parseInt((notifydate.getTime() / 1000))); // not doing the offset, might be a bug in umajin but does not seem to be needed - (notifydate.getTimezoneOffset() * 60));
*/

//var startDate = new Date('2020-10-24');
//print(startDate.getTime() / 1000);

/*
g_current_record_id = 1;
redcap_repeat_instance_number('sleep_date');
*/


