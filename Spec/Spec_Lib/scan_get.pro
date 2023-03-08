Function Scan_get, snum, header = head, status = stat, _extra = _e

;+
; NAME:
;		SCAN_GET
; VERSION:
;		7.15
; PURPOSE:
;		Reads the data of one scan from a SPEC data file.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_GET( SNUM [, keywords ])
; INPUTS:
;	SNUM
;		Positive integer, scan #, mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	HEADER
;		Optional output, see below
;	STATUS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to SPEC_FILE_INFO. All
;		SPEC_FILE_INFO keywords are accepted.  Not to be used directly.
; OUTPUTS:
;		Returns the scan data as a 2D array.
; OPTIONAL OUTPUT PARAMETERS:
;	HEADER
;		Returns the data header as a character vector.
;	STATUS
;		Returns, the read status, 1 for success, 0 for failure.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Reads the scan data using pointers provided in the
;		FILDAT structure (if needed, the structure is generated on the go).
;		Calls SPEC_FILE_INFO.  Also calls DEFAULT and STRPARSE_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 5-SEP-2002 by Mati Meron.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1
	res = 0b
	head = ''
	stat = 0

	if Default(snum,0l,/dtyp) gt 0 then begin
		Spec_file_info, _extra = _e
		if fildat.status and snum le fildat.nscan then begin
			if fildat.scan[snum].stat then begin
				openr, spcun, fildat.name, /get_lun
				line = ''
				point_lun, spcun, fildat.scan[snum].ptr[1]
				readf, spcun, line
				nhead = Strparse_mm(line,' 	',head) - 1
				head = head[1:*]
				sh = where(strlen(head) eq 1, nsh)
				while nsh gt 0 do begin
					j = sh[nsh-1]
					chk = strpos('0123456789',head[j])
					if chk ge 0 and j gt 0 then begin
						head[j-1] = strjoin(head[j-1:j])
						if j lt nhead then head[j:nhead-1] = head[j+1:nhead]
						nhead = nhead-1
						head = head[0:nhead]
					endif
					nsh = nsh-1
				endwhile
				nc = fildat.scan[snum].ncr[0]
				if nhead ge nc then head = head[nhead-nc+1:nhead]
				res = fltarr(fildat.scan[snum].ncr)
				point_lun, spcun, fildat.scan[snum].ptr[2]
				readf, spcun, res
				free_lun, spcun
				stat = 1
			endif else message, 'Missing scan or data!', /continue
		endif else message, 'No such file or scan!', /continue
	endif else message, 'Scan number must be positive!', /continue

	return, res
end