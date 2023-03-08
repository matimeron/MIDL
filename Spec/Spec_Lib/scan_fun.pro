Function Scan_fun, scan, fun, params = par, partial = prt, _extra = _e

;+
; NAME:
;		SCAN_FUN
; VERSION:
;		8.16
; PURPOSE:
;		Calculates a function of the SCAN data.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_DER( SCAN, FUN, [, keywords])
; INPUTS:
;	SCAN
;		Either a single valid scan, i.e. a [3,n] array, or a single scan number.
;		In the second case the scan will be read from the currently open SPEC
;		file.  Note that in the second case the appropriate data columns (see
;		SCAN_READ) need to be provided.
;
;		Note:	"partial" (2-column) scans are also acceptable, see PARTIAL.
;	FUN
;		Character value representing an existing IDL function.  The calling 
;		sequence for the function must be either
;			Result = FUN(x)
;		or
;			Result = FUN(x, extra)
;		where X is the variable and EXTRA may be any single entity (scalar,
;		array, or structure) used to pass additional parameters to the function.
;		In addition, the function may accept keywords.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
; 	PARAMS
;		An arbitrary value or variable which is passed to the function FUN.
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
;		Function evaluation using the IDL function CALL_FUNCTION.  For error
;		evaluation the derivative of the function is approximated using 
;		SCAN_DER.  Calls SCAN_LIST_VER and SCAN_READ (only when in the list 
;		mode).  Calls FPU_FIX from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JUN-2012 by Mati Meron.
;-

	on_error, 1

	nsc = Scan_list_ver(scan,flag=lfl,partial=prt,pflag=pfl,lis=slis)
	if nsc eq 0 then message, 'Missing or inconsistent input!' $
	else if nsc gt 1 then message, 'Only a single scan allowed!'
	if lfl then res = Scan_read(scan,_extra=_e) else res = scan

	if not pfl then tem = shift(res[0:1,*],1,0)
	if keyword_set(par) then $
	res[1,*] = FPU_fix(call_function(fun,res[1,*],par,_extra=_e)) else $
	res[1,*] = FPU_fix(call_function(fun,res[1,*],_extra=_e))

	if not pfl then begin
		tem[1,*] = res[1,*]
		df = Scan_der(tem,/part,_extra=_e)
		res[2,*] = df[1,*]*res[2,*]
	endif

	return, FPU_fix(res)
end