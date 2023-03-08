Pro User__define_old

;+
; NAME:
;		USER__DEFINE
; VERSION:
;		8.31
; PURPOSE:
;		Defines the {USER} sub-structure used with ADMIN data.
; CATEGORY:
;		Initialization.
; CALLING SEQUENCE:
;		None.
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Standard.  Defines the structure USER, including:
;
;			EXPID	:	An experiment ID number, long.
;			DATE	:	Experiment date in a month/day/year format, string.
;			BADGE	:	User badge number, long.
;			FNAME	:	User first name, string.
;			LNAME	:	User last name, string.
;			GENDER	:	User gender, string.
;			INSTIT	:	User's institution, string.
;			INSROL	:	User's institutional role, string.
;			ORGTYP	:	Organization type, string.
;			FUND	:	User's funding source, string.
;			SPOKESP	:	User's spokeperson status, 1-letter string ("Y" or "").
;			DISCIP	:	Experimental discipline, string.
;			TECH	:	Technique used, string.
;			PEN		:	Experiment's PEN number, string.
;			STATION	:	Experimental station used, 1-letter string.
;			MONTH	:	Month number, long.
;			DAY		:	Day number, long.
;			YEAR	:	Year number, long.
;			RUN		:	Run period number, long (values 1, 2 or 3).
; MODIFICATION HISTORY:
;		Created 15-JAN-2014 by Mati Meron.
;		Modified 20-MAY-2014 by Mati Meron.  Added fields.
;		Modified 20-SEP-2014 by Mati Meron.  Added fields.
;-

	on_error, 1

	dum = {user, expid: 0l, date: '', badge: 0l, fname:'', lname:'', gender:'',$
		instit: '', insrol: '', orgtyp: '', fund: '', spoksp: '', discip: '', $
		tech: '', pen: '', station: '', month: 0l, day: 0l, year: 0l, run: 0l}

	return
end