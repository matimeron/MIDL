Function Scan_list_ver, s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, $
	s_8, s_9, s_10, s_11, s_12, s_13, s_14, s_15, $
	check = chk, partial = prt, pflag = pfl, flag = lfl, list = sls, _extra = _e

;+
; NAME:
;		SCAN_LIST_VER
; VERSION:
;		8.08
; PURPOSE:
;		Verifies a provided list of scans.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_LIST_VER (S_0, ... [, keywords])
; INPUTS:
;	S_0, S_1, .. S_15
;		May be:
;			1)  Scans, i.e. [3,n] arrays.
;			2)	Scan numbers, i.e. integers
;			3)  A single array of scan numbers.
;			4)  A single character scalar or array to be processed by RANGE_PROC
;				from MIDL.  See there for allowed expressions.
;		In the first 2 cases the  maximal number of scans is 16.  In the third
;		and fourth case there is no limit.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/CHECK
; 		Switch.  If set, SCAN_LIST_VER checks whether the scan numbers 
; 		correspond to valid scans.  Active only in list mode.
; 	/PARTIAL
; 		Switch.   If set, "partial scans", i.e. [2,n] arrays are accepted.  Has
; 		no effect in list mode.
; 	PFLAG
; 		Optional output, see below.
;	FLAG
;		Optional output, see below.
;	LIST
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to RANGE_PROC.
; OUTPUTS:
;		Returns the number of scans present in the list.
; OPTIONAL OUTPUT PARAMETERS:
; 	PFLAG
; 		Returns 1 if /PARTIAL is set and "partial scans" are present, 0 
; 		otherwise.
;	FLAG
;		Returns 1 if S_0, ... are scan numbers (or an array of scan numbers)
;		0 otherwise.
;	LIST
;		If S_0, .. are scan numbers, LIST returns all the numbers as a single
;		array, else it is undefined.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls SCAN_VER (only when not in the list mode).  Uses
;		ARREQ, ISNUM, RANGE_PROC and TYPE from MIDL.
; MODIFICATION HISTORY:
;		Created 25-OCT-2002 by Mati Meron.
;		Modified 30-MAR-2003 by Mati Meron.  Added the character input option.
;		Modified 15-AUG-2009 by Mati Meron.  Increased maximal number of scans
;		from 8 to 16.
;		Modified 5-NOV-2009 by Mati Meron.  Added keywords PARTIAL and PFLAG.
;		Modified 20-AUG-2011 by Mati Meron.  Added keyword CHECK.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	snams =  strcompress('s_' + sindgen(16),/remove)

	ns = (pfl = (lfl = (sls = 0l)))

	for i = 0, n_params()-1 do dum = execute('ns=ns+(Type('+snams[i]+') gt 0)')
	if ns gt 0 then begin
		siz = (isn = (t = intarr(ns)))
		for i = 0l, ns - 1 do begin
			dum = execute('siz[i] = (size(' + snams[i] + '))[0]')
			dum = execute('isn[i] = Isnum(' + snams[i] + ')')
		endfor

		comp = Arreq(isn,t+1)* $
			(Arreq(siz,t) + 2*Arreq(siz,t+1) + 3*Arreq(siz,t+2))
		case comp of
			0	:	begin
						if ns eq 1 and Type(s_0) eq 7 then begin
							lfl = 1
							sls = Range_proc(s_0,_extra = _e)
							ns = n_elements(sls)
						endif else ns = 0l
					end
			1	:	begin
						lfl = 1
						sls = lonarr(ns)
						for i = 0l, ns - 1 do dum = $
						execute('sls[i] =' + snams[i])
					end
			2	:	begin
						if ns eq 1 then begin
							lfl = 1
							sls = s_0
							ns = n_elements(sls)
						endif else ns = 0l
					end
			3	:	begin
						svr = intarr(ns)
						for i = 0l, ns - 1 do dum = $
						execute('svr[i] = Scan_ver(' + snams[i] + ')')
						if keyword_set(prt) then begin
							pfl = 1l - Arreq(svr,svr mod 2)
							svr = svr mod 2
						endif
						ns = ns*Arreq(svr,t+1)
					end
		endcase
	endif

	if keyword_set(chk) and lfl and ns gt 0 then begin
		good = where(sls gt 0 and sls le fildat.nscan, ns)
		if ns gt 0 then begin
			sls = sls[good]
			good = where(fildat.scan[sls].stat mod 2, ns)
			if ns gt 0 then sls = sls[good] else sls = 0l
		endif else sls = 0l
	endif

	return, ns
end