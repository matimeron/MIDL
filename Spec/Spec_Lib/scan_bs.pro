Function Scan_BS, scan, back, partial = prt, _extra = _e

;+
; NAME:
;		SCAN_BS
; VERSION:
;		8.21
; PURPOSE:
;		Subtracts background from scan data.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_BS( SCAN, BACK, [, keywords])
; INPUTS:
;	SCAN
;		Either a single valid scan, i.e. a [3,n] array, or a single scan number.
;		In the second case the scan will be read from the currently open SPEC
;		file.  Note that in the second case the appropriate data columns (see
;		SCAN_READ) need to be provided.
;
;		Note:	"partial" (2-column) scans are also acceptable, see PARTIAL.
;	BACK
;		Two possibilities:
;			1)	Actual scan data (i.e. [3,N] array) which is subtracted from
;				SCAN.  Not allowed if /PARTIAL (see below) is set.
;			2)	A vector (scalar also acceptable) used as coefficient input
;				to POLEVAL (see there) for calculating polynomial background.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
; 	/PARTIAL
; 		Switch.   If set, "partial scans", i.e. [2,n] arrays are accepted.
;	_EXTRA
;		A formal keyword, used to transfer keywords to imbedded functions.  Not
;		to be used directly.
; OUTPUTS:
;		Normally returns an output in a scan format (i.e. a [3,n] array), where
;		the 3 columns (in order) are:
;			0	:	X values, unchanged from the original.
;			1	:	Y values, given by FUN(Y_original)
;			2	:	Y error values, given by appropriate scaling of the original
;			 		error values.
;
;		Note:	When /PARTIAL is set, the output has only the first two columns.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		If a scan number list is provided, SCAN_READ requires COLUMNS input.
;		See there for details.
; PROCEDURE:
;		Straightforward evaluation.  Calls SCAN_LC, SCAN_LIST_VER and SCAN_READ
;		(only when in the list mode).  Calls FPU_FIX and POLEVAL from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUL-2013 by Mati Meron.
;-

	on_error, 1

	nsc = Scan_list_ver(scan,flag=lfl,partial=prt,pflag=pfl,lis=slis)
	if nsc eq 0 then message, 'Missing or inconsistent input!' $
	else if nsc gt 1 then message, 'Only a single scan allowed!'
	if lfl then res = Scan_read(scan,_extra=_e) else res = scan

	wback = [reform(back)]
	siz = size(wback)
	case siz[0] of
		1	:	res[1,*] = res[1,*] - Poleval(res[0,*],back)
		2	:	if siz[1] eq 3 and not keyword_set(prt) $
				then res = Scan_lc(res,wback,coe=[1,-1],/inter) $
				else message, 'Bad background input!'
		else:	message, 'Bad background input!'
	endcase

	return, FPU_fix(res)
end