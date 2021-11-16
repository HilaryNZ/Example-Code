//start code

g_html_styles = "<style>\nBODY{\nfont-family: OpenSans-Regular.ttf;\nfont-size: 10pt;\ncolor: #111111;\n}\n\nH3{\nfont-family: OpenSans-SemiBold.ttf;\nfont-size: 12pt;\nfont-weight: bold;\n}\n\nSTRONG{\nfont-family: OpenSans-SemiBold.ttf;\nfont-weight: bold;\n}\n</style>\n";

g_redcap_token = '1B189E38A6CD9BA90DB00C2B1D396496';

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
g_repeat_instance = -1;          //TODO: Figure out how to tell


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

//
//custom action for login in App. Validating new user.
registerAction("redcap_newUser", "", "newUser form", "component:newUserform:form")  
   //the form component will get passed into my function
function redcap_newUser(form_comp) {
    g_newUserform_comp = form_comp;
    var jsonFormData = JSON.parse(gatherFormData(form_comp));
    print(jsonFormData)
    var recordID = jsonFormData.recordID.toUpperCase().trim(); 
    print('Record ID:' + recordID);
    var postbody = 'token=' + g_redcap_token + '&content=record&format=json&type=flat&fields=record_id&filterLogic=' + urlEncode('[record_id] = "' + recordID + '"');    
    print(postbody)
    httpPost("https://redcap.otago.ac.nz/api/", postbody, 'newUser_success', 'newUser_error');
}

// make custom action for subsiquent logins that skips login page and goes straight to Start Diary page 
registerAction("skip_login", "", "skip login", "") ;
function skip_login(){
    if(hasCookie("record_id")){
        var saved_record_id = parseInt(getCookie("record_id"));
        if(saved_record_id > 0){    //> 0
            print(saved_record_id)
            g_current_record_id = saved_record_id;
            load_globals();
            callAction("show_page_by_name","Start Diary",3);
        } else{
            print ("This App needs reinstalling")
        }
    }

}

// make custom action for standard login that checks against saved SUN_ID   
registerAction("redcap_login", "", "login form", "component:form:form")  
   //the form component will get passed into my function
function redcap_login(form_comp) {
    var jsonFormData = JSON.parse(gatherFormData(form_comp));
    print(jsonFormData)
    var recordID = jsonFormData.recordID.toUpperCase().trim(); 
    print('Record ID:' + recordID);
    
    //TODO: this needs to be moved to when the app opens the "Start Diary" page !!!!!!!!!!!!!!!!!!!!!!!
    var saved_record_id = getCookie("record_id");
    if (saved_record_id == recordID){
        // go to welcome page
        print(saved_record_id)
        load_globals();
        U_call_method("component_event", global_script_object, form_comp, 'on_send_success', '{}');
        //g_current_record = getCookie("user_record");
    } else {
        //did not match show a popup
        print("User not found")
        U_call_method("component_event", global_script_object, form_comp, 'on_send_error', '{"error": "Incorrect Participant ID"}');
        
    }
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
        print("id " + jsonResult)
        g_current_record_id =  jsonResult[0].record_id;
        print('g_current_record_id=' + g_current_record_id);
        setCookie("record_id", g_current_record_id);
        
        g_current_record = jsonResult;
        setCookie("user_record", result);
        print('Record ID:' + jsonResult[0].record_id)

        setCookie("redcap_answered_instruments", '[]'); //effectively set it to [], i.e. no answered surveys as far as the app knows
        
        // go to the welcome page
        U_call_method("component_event", global_script_object, g_newUserform_comp, 'on_send_success', '{}');
    }
}

function newUser_error(error_msg, status, result){
    if (result === null){
        result = '';
    }
    print('newUser_error (' + status + ')' + error_msg + ':' + result);
}

// TODO: Need to change this to disable page once the App is initialised 
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

// deleteCookie() delete the saved record_ID as an administrator.  So i can test that the SUN_ID is saving (i.e. re-enter as a new user).
//registerAction("deleteInformation", '', "Delete Saved Info", "component:username:text_entry");

//unction deleteInformation(username){
//    var user = getProperty(username,"text");
 //   deleteCookie(user);
//}

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
    setGlobalVar('datetime_DDMMYYYY_HHMM', formatDate(Date()) + ' ' + formatTime(Date()));
    print(formatTime(Date()))
    g_current_surveyName = surveyName;
    g_current_eventName = eventName;
    g_current_survey = null;
    g_current_question = -1; // TODO: Figure out where the user is upto in the survey 
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
        print('end survey');
    }else{
        var Question = g_current_survey[g_current_question];
        print('Question ' + g_current_question + ' field type=' + Question.field_type + ' field_name=' + Question.field_name + ' branching_logic='+Question.branching_logic);
        
        //TODO: Might need to move this up higher in case any kind of question has branch logic
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
        print('before fieldtype');
        switch(Question.field_type) {
            case 'radio':
                if(Question.field_annotation.includes('@photo')){ 
                    callAction("show_page_by_name","radio_pic",3);   //case 'text':  // skip text questions
                    print('radio_pic selected')   
                }else{     
                    callAction("show_page_by_name","radio",3);
                    print('radio selected')
                }
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
                        case 'datetime_dmy':
                            callAction("show_page_by_name","Date_Time",3);
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
    }     
        
}    

//custom action for submitting field data in a form into REDCAP
registerAction("redcap_start_submitSurvey", "", "submit Survey", "")  
//the form component will get passed into my function
function redcap_start_submitSurvey() {
    if (g_current_record == 0){
       print("record_id is " + g_current_record)
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
        var logic_string = g_current_survey[g_answer_index]['branching_logic'];
        
        var answerValue = g_answers[fieldname];
        
        var fieldannotation = g_current_survey[g_answer_index]['field_annotation'];
        print('fieldname=' + fieldname + ' answerValue=' + answerValue);
        //TODO: remove any answers if back button is used to nulify answers if branching logic == '0' (no)
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
    g_answered_surveys.push(g_current_surveyName + '|' + g_current_eventName);  //combine the survey and event names, to handle a survey that should be filled in over multiple events but is not "repeating" in that event
    setCookie('redcap_answered_instruments', JSON.stringify(g_answered_surveys));

    if (count_failed_submits() > 0){
        
        check_for_failed_submits();  //try to resend another oneq
    }else{
        callAction("show_page_by_name", 'Start Diary',3);
        deleteCookie(g_current_surveyName + '_answer');   // delete previous answers so they don't reappear in next repeated survey if branching logic appears    
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



///////////////////////////////////////////////////////////////////////////////////////

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
registerAction("redcap_fillradio", "", "fill radio in Survey", "component:fillradio:html_article,component:but1:button,component:but2:button,component:but3:button,component:but4:button,component:but5:button,component:but6:button,component:but7:button")  
  
function redcap_fillradio(html_comp,but1,but2,but3,but4,but5,but6,but7) {
    var saved_surveyData = getCookie(g_current_surveyName);
    var survey = JSON.parse(saved_surveyData);
    var Question = survey[g_current_question];
    print('field type=' + Question.field_type);
    
    //set HTML on htmp article
    setProperty(html_comp, 'text', g_html_styles + Question.field_label);
    // not sure if removing '0' will be a problem.
    //"0, Type 1 | 1, Type 2 | 2, Type 3 | 3, Type 4 | 4, Type 5 | 5, Type 6 | 6, Type 7"
    // split string by "|" and "," then trim.
   
    var field_list =  Question.select_choices_or_calculations.split("|");
    //print(field_list[0])
 
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
        var label6 = field_list[6].trim().split(',')[1].trim();
		setProperty(but7,'text', label6);
        setProperty(but7, 'visible','True')
	} else{
		setProperty(but7, 'visible','False')
    }
}

// Fill text question from the survey
registerAction("redcap_filltext", "", "fill text in Survey", "component:html:html_article,component:field_label:html_article,component:text:text_entry") 

function redcap_filltext(html_comp, html_comp_label, text_comp) {
    var saved_surveyData = getCookie(g_current_surveyName);
    var survey = JSON.parse(saved_surveyData);
    var Question = survey[g_current_question];
    print('field type=' + Question.field_type);
    
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
    
    //TODO: Use a global for the answers
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
            //TODO : go away and get data
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
    
    print('get_days_passed=' , day_num);
    return day_num;
}

// custom survey feed, need to get the legit events and then show the surveys not in the exception list
registerFeed('redcap_surveyfeed', 'Survey Feed', ''); //name of function, description of function

function redcap_surveyfeed(component) {
    var survey_output = get_active_surveys();
    setCustomFeed(component, JSON.stringify(survey_output));
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
    httpPost("https://redcap.otago.ac.nz/api/", 'token=' + g_redcap_token + '&content=instrument&format=json&returnFormat=json', "instrument_success", "instrument_failure");
    print('get Instruments');
} 

function instrument_success(returnvalue, responseHeaders, cookieJar ){
    //print('Get Event Mapping success: ' + returnvalue);
    //TODO:Check the result looks like valid json and not rando html from a captive portal
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
    httpPost("https://redcap.otago.ac.nz/api/", 'token=' + g_redcap_token + '&content=formEventMapping&format=json&returnFormat=json', "eventMapping_success", "eventMapping_failure");
    print('get Event Mapping')
} 

function eventMapping_success(returnvalue, responseHeaders, cookieJar ){
    //print('Get Event Mapping success: ' + returnvalue);
    //TODO:Check the result looks like valid json and not rando html from a captive portal
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
    httpPost("https://redcap.otago.ac.nz/api/", 'token=' + g_redcap_token + '&content=repeatingFormsEvents&format=json&returnFormat=json', "repeating_instruments_success", "repeating_instruments_failure");
    print('get Repeating Forms and Events')
} 

function repeating_instruments_success(returnvalue, responseHeaders, cookieJar ){
    print('repeating_instruments_success: ' + returnvalue);
    //TODO:Check the result looks like valid json and not rando html from a captive portal
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
    httpPost("https://redcap.otago.ac.nz/api/", 'token=' + g_redcap_token + '&content=event&format=json&returnFormat=json', "events_success", "events_failure");
    print('get events')
} 

function events_success(returnvalue, responseHeaders, cookieJar ){
    //print('Get events success: ' + returnvalue);
    //TODO:Check the result looks like valid json and not rando html from a captive portal
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
        httpPost("https://redcap.otago.ac.nz/api/", 'token=' + g_redcap_token + '&content=metadata&format=json&returnFormat=json&forms[0]=' + urlEncode(surveyName), "survey_success", "survey_failure");
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

function finish_download(initialSetup){
    print('finish download all the stuff');
    setGlobal('http_spinner', 'True');
    callAction("show_page_by_name",g_download_nextpage,3); 
}

function redcap_repeat_instance_number(fieldName) {
    print('redcap_repeat_instance_number=' + fieldName);
    //find the first field in the form
    var postbody = 'token=' + g_redcap_token + '&content=record&type=eav&format=json&filterLogic=' + urlEncode('[record_id]=' + g_current_record_id) + '&fields[0]=' + urlEncode(fieldName);
    print('postbopdy=' + postbody);
    httpPost("https://redcap.otago.ac.nz/api/", postbody, "repeat_num_success", "repeat_num_failure");
} 

function repeat_num_success(returnvalue, responseHeaders, cookieJar ){
    print('Get repeat num success: ' + g_current_surveyName + ' ' + returnvalue);
    //TODO:Check the result looks like valid json and not rando html from a captive portal
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
    return instrument_name;  
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

function survey_backbutton(returnvalue){
    print('survey_backbutton');
    g_current_question = g_current_question - 2;  // g_current_question --; is the shortcut for this line
    
    redcap_showSurvey();
    //callAction("navigate_back");
}


/*// TODO:  Up to here:
//Go get the eventInstrumentMapping, get the events and get the instruments (i.e. metadata)
registerAction("survey_backbutton", "", "Go back a page", "");

function survey_backbutton(returnvalue){
    print('survey_backbutton');
    g_current_question = g_current_question - 2;  // g_current_question --; is the shortcut for this line
    
    for(){
        if (branching_logic(Question.branching_logic)){
            //show it
        }

    } 
    redcap_showSurvey();
    //callAction("navigate_back");
}
/*
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
*/
// custom action DateTime format
registerAction("date_format", "", "Format the Date", "component:dateFormat:text_entry")

function date_format(text_comp){
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
            break;
        }
    }
}

function isDigit(n) {
    return Boolean([true, true, true, true, true, true, true, true, true, true][n]);
}

//TODO: lock button until correct time format entered
//TODO: limit input to correct range of time (24hr clock) 
// custom action 
registerAction("time_format", "", "Format the Time", "component:TimeFormat:text_entry")

function time_format(text_comp){
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
            break;
        }
    }
   
    
}

registerAction("date_time_format", "", "Format the Date Time", "component:DateTime:text_entry,component:but:button")

function date_time_format(text_comp,button_comp){
    var format = getProperty(text_comp, 'text')
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
        //increment space if index == 9 place '-'
        if (output.length == 10){
            output += ' '
        }
        //increment space if index == 12 place '-'
        if(output.length == 13){   
            output += ':'; 
        }
        //increment space if index == 6 place '-'
        if (output.length >= 16){
            print('output=[' + output + ']');
            setProperty(text_comp, 'text', output);
            print('button is enabled ' + output.length);
            callAction('set_button_enabled', button_comp, true);
            return;
        }
       
       
       
    }
    //print(output) break;
    //if we get here then it is not right
    callAction('set_button_enabled', button_comp, false);
    print('button is disabled ' + output.length);
}
