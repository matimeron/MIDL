Function Scan_MCA_read, snum, bin = bin, $
	monc = mnc, calib = clb, qz = qzv, energy = enr, angle = ang, _extra=_e

;+
; NAME:
;		SCAN_MCA_READ
; VERSION:
;		7.15
; PURPOSE:
;		Reads a set of MCA spectra from a scan in a SPEC data file.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_MCA_READ( SNUM [, keywords])
; INPUTS:
;	SNUM
;		Scan number.  Only a single number is acceptable.
; OPTIONAL INPUT PARAMETERS:
;		None
; KEYWORD PARAMETERS:
;	BIN
;		Bin size.  Default value is 1
;	MONC
;		Optional output, see below.
;	CALIB
;		Optional output, see below.
;	QZ
;		Optional output, see below.
;	ENERGY
;		Optional output, see below.
;	/ANGLE
;		Switch.  Modifies the QZ output.  See below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to SPEC_FILE_INFO
; OUTPUTS:
;		Returns an array (type LONG) of dimension [NMCA,NCHAN] where NMCA is
;		the number of MCA spectra in the scan and NCHAN is the number of points
;		in a single spectrum.
; OPTIONAL OUTPUT PARAMETERS:
;	MONC
;		Returns a scalar value, the value of the normalization counter
;		(traditionally named MONC).
;	CALIB
;		Returns a 2 element vector containing the energy calibration constants,
;		in [zero-intercept,slope] order.
;	QZ
;		By default returns the QZ values for the spectra as vector of dimension
;		NMCA.  If /ANGLE is set, instead of QZ the values of "angle" (by
;		convention, the first column of the scan) are returned instead.
;	ENERGY
;		Returns the energy values as vector of dimension NCHAN.  The values are
;		obtained from channel numbers using CALIB.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		The value of BIN can be changed during processing.  The final value will
;		be the the smaller of the original value and the highest power of 2
;		which divides the number of channels of the LD spectrum.  Default is 1..
; RESTRICTIONS:
;		The scan number SNUM must correspond to a valid scan.
; PROCEDURE:
;		Straightforward, reads the data using parameters stored in the FILDAT
;		structure in the common block.  Calls SCAN_FIELD_READ, SCAN_GET and
;		SPEC_FILE_INFO.
;		Also calls ISNUM, POLEVAL, STREQ and STRPARSE_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 20-MAY-2004 by Mati Meron.
;		Modified 10-OCT-2005 by Mati Meron.  Added keyword ANGLE.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	case n_elements(snum) of
		0	:	message, 'Missing scan number!'
		1	:	wnum = snum
		else:	message, 'One scan at a time!
	endcase
	Spec_file_info, _extra = _e
	if wnum le 0 or wnum gt fildat.nscan then message, $
	'Scan number out of range!'

	if Scan_field_read(wnum,'mcstat') then begin
		n = (Scan_field_read(wnum,'mcnum'))[0]
		nchan = (Scan_field_read(wnum,'mchan'))[0]
		sing = lonarr(nchan)
		res = lonarr(n,nchan)
		openr, spcun, fildat.name, /get_lun
		for i = 0l, n-1 do begin
			point_lun, spcun, fildat.scan[wnum].mcptr[i]
			readf, spcun, sing
			res[i,*] = sing
		endfor
		free_lun, spcun
		shead = (Scan_field_read(wnum,'shead'))[0]
		dum = Strparse_mm(shead,' 	',lis)
		mnc = abs(long(lis[dum]))
		clb = [fildat.scan[wnum].mcint,fildat.scan[wnum].mcslp]
		if arg_present(qzv) then begin
			sdat = Scan_get(wnum,head=hed)
			if not keyword_set(ang) then begin
				dum = where(Streq(hed,'L'),ndum)
				if ndum eq 1 then qzv = reform(sdat[dum,*]) $
				else message, 'Cannot find Qz column', /cont
			endif else qzv = reform(sdat[0,*])
		endif
		if arg_present(enr) then enr = Poleval(lindgen(nchan),clb)
	endif else message, 'No spectral data present!'

	if Isnum(bin) then begin
		bin = 2l^(floor(alog(bin>1)/alog(2)))
		repeat begin
			check = (bin*(nchan/bin) eq nchan)
			if check then break else bin = bin/2
		endrep until check
		res = reform(res,n,bin,nchan/bin)
		res = long(total(res,2))
		if Isnum(enr) then enr = enr[bin/2 + bin*lindgen(nchan/bin)]
	endif else bin = 1l

	return, res
end