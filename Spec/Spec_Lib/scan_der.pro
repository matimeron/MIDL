Function Scan_der, scan, partial = prt, _extra = _e

;+
; NAME:
;		SCAN_DER
; VERSION:
;		8.23
; PURPOSE:
;		Calculates numeric derivative approximation for the SCAN data.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_DER( SCAN [, keywords])
; INPUTS:
;	SCAN
;		Either a single valid scan, i.e. a [3,n] array, or a single scan number.
;		In the second case the scan will be read from the currently open SPEC
;		file.  Note that in the second case the appropriate data columns (see
;		SCAN_READ) need to be provided.
;
;		Note:	"partial" (2-column) scans are also acceptable, see PARTIAL.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
; 	/PARTIAL
; 		Switch.   If set, "partial scans", i.e. [2,n] arrays are accepted.
;	_EXTRA
;		A formal keyword, used to transfer keywords to SCAN_READ and DIF.  Not
;		to be used directly.
; OUTPUTS:
;		Normally returns an output in a scan format (i.e. a [3,n] array), where
;		the 3 columns (in order) are:
;			0	:	X values, unchanged from the original.
;			1	:	Y values, given by numeric derivatives of the original
;					Y values.
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
;		Numeric differentiation using the function DIF.  By default, centered
;		differences are used, this can be changed using the DIF keywords.  Calls
;		SCAN_LIST_VER and SCAN_READ (only when in the list mode).  Calls DIF, 
;		FPU_FIX and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JUN-2012 by Mati Meron.
;		Modified 5-FEB-2014 by Mati Meron.  Changed default difference mode from
;		"backward" to "centered".
;-

	on_error, 1

	nsc = Scan_list_ver(scan,flag=lfl,partial=prt,pflag=pfl,lis=slis)
	if nsc eq 0 then message, 'Missing or inconsistent input!' $
	else if nsc gt 1 then message, 'Only a single scan allowed!'
	if lfl then res = Scan_read(scan,_extra=_e) else res = scan

	if ((Wherinstruct('ba',_e))[0] ge 0) or ((Wherinstruct('fo',_e))[0] ge 0) $
	then cen = 0 else cen = 1 

	dx = Dif(res[0,*],/lin,cen=cen,_extra=_e)
	df = Dif(res[1,*],/lin,cen=cen,_extra=_e)
	res[1,*] = df/dx
	if not pfl then begin
		if cen then co = 1/sqrt(2) else co = sqrt(2)  
		res[2,*] = co*res[2,*]/dx
	endif

	return, FPU_fix(res)
end