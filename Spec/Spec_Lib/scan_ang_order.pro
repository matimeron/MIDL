Function Scan_ang_order, s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, $
	order_by = orb, list = slis, _extra = _e

;+
; NAME:
;		SCAN_ANG_ORDER
; VERSION:
;		7.15
; PURPOSE:
;		Orders scans by detector column angles.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_ANG_ORDER(S_0, ...  [, keywords])
; INPUTS:
;	S_0, S_1, .. S_7
;		May be:
;			1)	Scan numbers, i.e. integers
;			2)  A single array of scan numbers.
;			3)  A single character scalar or array to be processed by RANGE_PROC
;				from MIDL.  See there for allowed expressions.
;
;		In the first case the maximal number of scans is 8.  In the second and
;		third case there is no limit.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ORDER_BY
;		Character input (only first letter matters), determines the sorting
;		parameter.  The 2 options currently defined are:
;
;			1)	BETA-	Sorts scans in order of the BETA ('OUT_ROT') angle
;						followed by, in case ambiguity remains, by a sort on
;						the value of the DTH ('DET_TH') angle.
;			2)	DTH	-	Reverses the procedure above, DTH first, BETA, if needed
;						second.
;		The default option is the first one, BETA.
;	LIST
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to SCAN_LIST_VER.
; OUTPUTS:
;		Returns a long integer array specifying the order of the scans, same as
;		SORT.
; OPTIONAL OUTPUT PARAMETERS:
;	LIST
;		Returns the complete list of scans, as combined by ARRPACK and processed
;		by SCAN_LIST_VER.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The scan numbers must correspond to valid scan numbers in a currently
;		open SPEC file.
; PROCEDURE:
;		Straightforward.  Calls SCAN_LIST_VER.  Calls ARRPACK, LEXISORT and
;		STRMATCH_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 20-OCT-2007 by Mati Meron.
;		Modified 15-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	posib = ['beta','dth']
	whi = Strmatch_mm(orb,posib,1) > 0
	snum = Arrpack(s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7)
	nsc = Scan_list_ver(snum,lis=slis,_extra=_e)
	if nsc eq 0 then message, 'Missing or inconsistent input!'

	bet = reform((fildat.scan[slis].angs)[1,*])
	dth = reform((fildat.scan[slis].angs)[2,*])

	if whi eq 0 then res = Lexisort(bet,dth) else res = Lexisort(dth,bet)

	return, res
end