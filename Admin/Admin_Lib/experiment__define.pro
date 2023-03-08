Pro Experiment__define

;+
; NAME:
;		EXPERIMENT__DEFINE
; VERSION:
;		8.31
; PURPOSE:
;		Defines the {EXPERIMENT} sub-structure used with ADMIN data.
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
;			PEN		:	Experiment's PEN number, string.
;			STATION	:	Experimental station used, 1-letter string.
;			DATE	:	Experiment date in a month/day/year format, string.
;			MONTH	:	Month number, long.
;			DAY		:	Day number, long.
;			YEAR	:	Year number, long.
;			RUN		:	Run period number, long (values 1, 2 or 3).
;			SPOKFN	:	Spokeperson's first name, string.
;			SPOKLN	:	Spokeperson's last name, string.
;			FUND	:	Experiment's funding source, string.
;			NNAMES	:	Number of user names (thus users), long.
;			NAMES	:	Names of the participating users, string array.
;			NDISCIP	:	Number of disciplines pertaining to the experiment, long
;			DISCIP	:	List of the disciplines, string array.
;			NTECH	:	Number of the techniques used, long.
;			TECH	:	List of the techniques, string array.
; MODIFICATION HISTORY:
;		Created 15-JAN-2014 by Mati Meron.
;		Modifed 20-MAY-2014 by Mati Meron.  Added fields.
;-

	on_error, 1

	dum = {experiment, expid: 0l, pen: '',  station: '', $
		date: '', month: 0l, day: 0l, year: 0l, run: 0l, $
		spokfn: '', spokln: '', fund: '', nnames: 0l, names: strarr(32), $
		ndiscip: 0l, discip: strarr(8), ntech: 0l, tech: strarr(8)}

	return
end