Function Scan_LD_read, snum, norm = nrm, chan_range= chr, bin= bin, list= sls,$
	 nvec = nvec, _extra=_e

;+
; NAME:
;		SCAN_LD_READ
; VERSION:
;		7.15
; PURPOSE:
;		Reads a set of Linear Detector spectra from a SPEC data file.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_LD_READ( SNUM [, keywords])
; INPUTS:
;	SNUM
;		Scan list.  Either an array of integers or a character scalar or array
;		of any form acceptable by SCAN_LIST_VER.
; OPTIONAL INPUT PARAMETERS:
;		None
; KEYWORD PARAMETERS:
;	NORM
;		Specifies data normalization.  If NORM is set (with nonzero numeric
;		value) the data is normalized to MONC.  This can be overriden by
;		giving NORM as character value in which case this value specifies
;		the column to be used for normalization.
;	CHAN_RANGE
;		The range of channels to use in the readout.  Default is full range
;		present in the data.
;	BIN
;		Bin size.  Default value is 1
;
;		Note:	When both CHAN_RANGE and BIN are present, the CHAN_RANGE values
;				are taken as pre-binned values.
;	LIST
;		Optional output, see below.
;	NVEC
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to RANGE_PROC
;		through SCAN_LIST_VER, and to SPEC_FILE_INFO.
; OUTPUTS:
;		Returns an array (type LONG) of dimension [NSCAN,NCHAN,2] where NSCAN is
;		the number of scans and NCHAN is the number of points in a single LD
;		spectrum.  The [NSCAN,NCHAN,0] part of the output contains the scan data
;		and the [NSCAN,NCHAN,1] part contains the statistical errors.
; OPTIONAL OUTPUT PARAMETERS:
;	LIST
;		Returns the list of all scans proscribed by SNUM as a long integer
;		array, for the use of calling routines.
;	NVEC
;		Returns the normalization vector (only when /NORM is set).
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		The value of BIN can be changed during processing.  The final value will
;		be the the smaller of the original value and the highest power of 2
;		which divides the number of channels of the LD spectrum.
; RESTRICTIONS:
;		All the scans accessed must be valid scans containing a linear detector
;		output of same length NCHAN.
; PROCEDURE:
;		Straightforward, reads the data using parameters stored in the FILDAT
;		structure in the common block.  Calls SCAN_COLUMN, SCAN_LIST_VER and
;		SPEC_FILE_INFO.  Also calls ARREQ and ISNUM from MIDL.
; MODIFICATION HISTORY:
;		Created 1-APR-2003 by Mati Meron.
;		Modified 15-AUG-2003 by Mati Meron.  Added keyword BIN.
;		Modified 25-NOV-2005 by Mati Meron.  Added keywords CHAN_RANGE and LIST.
;		Modified 20-DEC-2005 by Mati Meron.  Added keyword NORM.
;		Modified 20-FEB-2006 by Mati Meron.  Added keyword NVEC.
;		Modified 20-JUN-2006 by Mati Meron.  Changed processing and output
;		format to include statistical errors.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	n = Scan_list_ver(snum,list=slis,flag=sfl,_extra=_e)
	if n gt 0 and sfl then begin
		Spec_file_info, _extra = _e
		check = Arreq([slis gt 0 and slis le fildat.nscan],replicate(1b,n))
		if fildat.status and check then begin
			nchan = max(fildat.scan[slis].chan,min=comp)
			if nchan gt 0 and nchan eq comp then begin
				openr, spcun, fildat.name, /get_lun
				sing = lonarr(nchan)
				res = fltarr(n,nchan,2)
				if keyword_set(nrm) then begin
					if Type(nrm) eq 7 then ncol = nrm $
					else if nrm eq 1 then ncol = 'monc'
					nvec = fltarr(n)
					nstat = intarr(n)
					nfl = 1
				endif else nfl = 0
				for i = 0l, n-1 do begin
					point_lun, spcun, fildat.scan[slis[i]].ptr[3]
					readf, spcun, sing
					res[i,*,0] = (res[i,*,1] = sing)
					if nfl then begin
						nvec[i] = 1/Scan_column(slis[i],ncol,/con,stat=st)
						if st then begin
							res[i,*,0] = nvec[i]*res[i,*,0]
							res[i,*,1] = nvec[i]^2*res[i,*,1]
						endif else message, 'Bad normalization data!'
					endif
				endfor
				free_lun, spcun
				sls = slis
			endif else message, 'Data length incompatibility!'
		endif else message, 'No such file or scan(s)!'
	endif else message, 'Scan numbers?'

	if Isnum(bin) then begin
		bin = 2l^(floor(alog(bin>1)/alog(2)))
		if bin gt 1 then begin
			repeat begin
				check = (bin*(nchan/bin) eq nchan)
				if check then break else bin = bin/2
			endrep until check
			nchan = nchan/bin
			res = reform(res,n,bin,nchan,2)
			res = total(res,2)
		endif
	endif else bin = 1l

	if Isnum(chr) then begin
		wchr = 0 > ([long(chr)/bin,nchan-1])[0:1] < (nchan-1)
		res = res[*,wchr[0]:wchr[1],*]
	endif

	res[*,*,1] = sqrt(res[*,*,1])

	return, res
end