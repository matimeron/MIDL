Pro Spec_file_check, snum, single= sng, pildet= pdt, lindet= ldt, mcadet= mdt, $
	close_only = cln, par_const = pcs, field_const = fcs, warn_only = won, $
	nscan = nsc, list = lis, _extra = _e

;+
; NAME:
;		SPEC_FILE_CHECK
; VERSION:
;		8.01
; PURPOSE:
;		Checks on the status of a SPEC file, initiating readout if needed.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		SPEC_FILE_CHECK, SNUM [, keywords ]
; INPUTS:
;	SNUM
;		Scan number or a list of scan numbers, in any form acceptable by
;		SCAN_LIST_VER.  A value of 0 translates to "all scans in the file".
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/SINGLE
;		Switch.  If set, only a single scan is allowed.
;	/PILDET														|
;		Switch.  If set, a check is performed to verify that a 	|
;		Pilatus detector is defined for all the scans in SNUM.	|	At most one
;	/LINDET														|	of these
;		Switch.  If set, a check is performed to verify that a 	|	three may
;		linear detector is defined for all the scans in SNUM.	|	be defined.
;	/MCADET														|
;		Switch.  If set, a check is performed to verify that 	|
;		an MCA spectrum is defined for all the scans in SNUM.	|
;		
;		Note:	/PILDET actually checks for the presence of any area detector
;				not just Pilatus.
;	/CLOSE_ONLY
;		Switch.  If set, none of the scans in snum may use a far detector.
;	PAR_CONST
;		Character scalar or array translating to a name(s) of parameter(s)
;		defined for the current SPEC file.  If exists, a check is performed to
;		verify that said parameter(s) is(are) constant for all scans in SNUM.
;		If not, an error message is issued.
;	FIELD_CONST
;		Character scalar or array translating to a name(s) of field(s) defined
;		in SPSCAN__DEFINE.  If exists, a check is performed to verify that said
;		field(s) is(are) constant for all scans in SNUM.  If not, an error
;		message is issued.
;	/WARN_ONLY
;		Switch.  If set and if a parameter and/or field is not constant, a
;		warning message is issued instead of an error message.
;	NSCAN
;		Optional output, see below.
;	LIST
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		None, other than possible screen output.
; OPTIONAL OUTPUT PARAMETERS:
;	NSCAN
;		Returns the numbers of scans defined by SNUM.
;	LIST
;		Returns an array of the scan numbers defined by SNUM.
; COMMON BLOCKS:
;		SPEC_FILE.  See SPEC_FILE_INFO for details.
; SIDE EFFECTS:
;		None other than possible modification of the common block.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls SCAN_FIELD_READ, SCAN_PAR_READ and
;		SPEC_FILE_INFO.  Calls ARREQ, CURRENT, ONE_OF, TYPE and WHERINSTRUCT 
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 20-MAY-2008 by Mati Meron.
;		Modified 10-JUN-2008 by Mati Meron.  Addded keyword /SINGLE.
;		Modified 20-MAR-2010 by Mati Meron.  Slight internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 20-FEB-2011 by Mati Meron.  Added keyword CLOSE_ONLY.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1
	nl = string([10b,9b])
	wfl = keyword_set(won)

	Spec_file_info, _extra = _e
	dum = (Wherinstruct('new',_e))[0]
	if dum ge 0 then _e.(dum) = 0
	if fildat.status then begin
		if n_elements(snum) gt 0 then begin
			nsc = Scan_list_ver(snum,flag=sfl,lis=lis)
			etext = nl + 'Bad scan number(s), available scan numbers are ' + $
			strjoin(string([1,fildat.nscan], form = '(i0)'),'-') + '!' + nl
			if nsc gt 0 and sfl then begin
				if Arreq(lis,[0]) then begin
					nsc = fildat.nscan
					lis = 1l + lindgen(nsc)
				endif else begin
					smin = min(lis,max=smax)
					if smin le 0 or smax gt fildat.nscan then message,etext,/non
				endelse
			endif else message, etext, /non
			if keyword_set(sng) then begin
				if nsc eq 1 then lis = lis[0] $
				else message, 'Only single scan allowed!'
			endif
		endif else message, 'Missing scan number(s)!'
	endif else message, 'No file or no data in file!'

	case One_of(pdt,ldt,mdt) of
		-1	:
		0	:	begin
					stat = min(Scan_field_read(lis,'PDstat') and 1)
					if not stat then message, $
					nl + 'No PD data in some/all scans!' + nl, /non
				end
		1	:	begin
					stat = min(Scan_field_read(lis,'Lindet') and 1)
					if not stat then message, $
					nl + 'No LD data in some/all scans!' + nl, /non
				end
		2	:	begin
					stat = min(Scan_field_read(lis,'MCstat') and 1)
					if not stat then message, $
					nl + 'No MCA data in some/all scans!' + nl, /non
				end
	endcase

	if keyword_set(cln) then begin
		nstat = max(Scan_field_read(lis,'PDfar'))
		if nstat then message, 'The routine ' + Current(call=1) + $
		' cannot be used with far detector!'
	endif

	if n_elements(pcs) gt 0 and Type(pcs) eq 7 then begin
		dum = where(pcs ne '',ndum)
		if ndum gt 0 then begin
			wpcs = pcs[dum]
			check = Scan_par_read(lis,wpcs,/con,cflag=cfl,_extra=_e)
			dum = where(cfl eq 0,ndum)
			if ndum gt 0 then message, nl+ strupcase(Strjoin(wpcs[dum],', '))+ $
			' not constant!' + nl, con = wfl
		endif
	endif

	if n_elements(fcs) gt 0 and Type(fcs) eq 7 then begin
		dum = where(fcs ne '',ndum)
		if ndum gt 0 then begin
			wfcs = fcs[dum]
			cfl = intarr(n_elements(wfcs))
			for i = 0l, n_elements(wfcs)-1 do begin
				dum = Scan_field_read(lis,wfcs[i],/con,cflag=cflg,_extra=_e)
				cfl[i] = cflg
			endfor
			dum = where(cfl eq 0,ndum)
			if ndum gt 0 then message, nl+ strupcase(Strjoin(wfcs[dum],', '))+ $
			' not constant!' + nl, con = wfl
		endif
	endif

	return
end