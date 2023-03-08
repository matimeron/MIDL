Pro Optconv, wavl, ssize= ssz, asize= asz, zfsor= zfs, hsize= hsz, colen= cln,$
	mask= msk, internal= int, relative= rel, converging=cnv, near= ner, far= far

;+
; NAME:
;		OPTCONV
; VERSION:
;		6.4
; PURPOSE:
;		Conversion between various sets of beam parameters.
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		OPTCONV, WAVL, SSIZE=SSZ, ASIZE=ASZ, ZFSOR=ZFS, HSIZE=HSZ, COLEN=CLN, $
;			[keywords]
; INPUTS:
;	WAVL
;		Wavelength.  Mandatory.  Given in Angstrem (or meter if /INTERNAL is
;		set).
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	SSIZE
;		Source spatial size.
;	ASIZE
;		Source angular size.
;	ZFSOR
;		Distance from source.
;	HSIZE
;		Transverse beam size.
;	COLEN
;		Transverse coherence length.
;
;		Note:   3 and only 3 of the above parameters must be given.  However,
;				see /MASK below.
;
;	MASK
;		An optional five element vector which can be used to activate and
;		deactivate optical parameters from the list of five above (in order).
;		A non-zero entry in mask means "active", zero means "non-active".
;
;		Alternatively, the mask can also be provided as a character array,
;		specifying explicitly the three inputs to be used, out of the list
;		['SSIZE','ASIZE','ZFSOR','HSIZE','COLEN'].  Only the first 2 characters
;		are needed and case or order do not matter.
;
;		Note:	When MASK is used, one can have more than 3 defined inputs.
;				The number of inputs which are both "defined" and "accepted"
;				(i.e. corresponding to nonzero MASK entries) must still be 3.
;
;	/INTERNAL
;		Switch.  If set, the values of WAVL, SSIZE, HSIZE, COLEN, are given
;		in meters, and ASIZE in radians.  Else (default), WAVL is given in
;		angstrem, SSIZE, HSIZE, COLEN in microns and ASIZE in microradians.
;	/RELATIVE
;		Switch.  If set, COLEN represents relative (to HSIZE) coherence.
;		ALLOWED only when HSIZE is given or when COLEN is not given.
;	/CONVERGING
;		Switch.  Specifies that the beam is converging, i.e. the calculated
;		ZFSOR value is negative.  When ZFSOR is provided as input, not
;		calculated, /CONVERGING has no effect.
;	/NEAR
;		Switch, specifying that the "near" solution is to be picked.  Matters
;		only for the case where the provided parameters are ZFSOR, HSIZE and
;		COLEN.  In this case two different solutions are possible for SSIZE
;		and ASIZE.  One is the "near" solution, where ZFSOR*ASIZE <= SSIZE,
;		the other is the "far" one where ZFSOR*ASIZE >= SSIZE.  Setting /NEAR
;		selects the "near" solution.
;	/FAR
;		Similar to /NEAR, selects the "far" solution.
;
;		Note:	Only one of /NEAR and /FAR may be set.  If none is, the default
;				is /NEAR.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		As mentioned above, 3 out of the 5 parameter (SSIZE, ASIZE, ZFSOR,
;		HSIZE and COLEN) are to be provided.  Whichever 3 are provided, the
;		remaining 2 serve as output parameters.
; COMMON BLOCKS:
;		Block BEAM_STUFF, as defined in INIT_OPT.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Other than the basic restriction (only 3 out of the five paremeters
;		are to be defined), there is a set of restrictions on the parameters,
;		stemming from their optical meaning, such as:
;
;		1)  All parameters other than ZFSOR have to be positive.
;		2)	HSIZE >= SSIZE
;		3)	HSIZE >= ZFSOR*ASIZE
;		4)	HSIZE >= COLEN
;
;		Additional restrictions, depending on the identity of the provided
;		parameters.
; PROCEDURE:
;		Standard optical calculations, depending on the parameters provided.
;		In all cases the 3 defined parameters are used to calculate the other 2.
;		The solution is usually unique, except for the last case, where ZFSOR,
;		HSIZE and COLEN are provided.  In this case two different solutions are
;		possible and the keywords /NEAR and /FAR can be used to select between
;		them.
;		Calls ARREQ, CAST, ISNUM, ONE_OF and TYPE from MIDL.
; MODIFICATION HISTORY:
;		Created 15-DEC-2002 by Mati Meron.
;		Modified 10-OCT-2007 by Mati Meron.  Added keyword CONVERGING and
;		extended MASK to accept character inputs.
;-

	common beam_stuff, exs, npomax, mmicr, mwlen
	on_error, 1
	posib = ['ssize','asize','zfsor','hsize','colen']

	Opt_init
	if not keyword_set(int) then begin
		wavfac = mwlen
		sizfac = mmicr
	endif else wavfac = (sizfac = 1.)
	if Isnum(wavl) then begin
		if wavl gt 0 then wwavl = wavfac*wavl $
		else message,'Wavelength must be positive!'
	endif else message, 'Wavelength must be provided!'

	if Type(msk) eq 7 then begin
		wmsk = bytarr(5)
		for i = 0, n_elements(msk) - 1 do begin
			j = Strmatch_mm(msk[i],posib,2)
			if j ge 0 then wmsk[j] = 1
		endfor
	endif else begin
		if Isnum(msk) then wmsk = (msk ne 0) else wmsk = replicate(1b,5)
	endelse
	list = (nflg = bytarr(5))

	if Isnum(ssz) then begin
		list[0] = wmsk[0]
		nflg[0] = (ssz le 0)*wmsk[0]
		wssz = sizfac*ssz
	endif
	if Isnum(asz) then begin
		list[1] = wmsk[1]
		nflg[1] = (asz le 0)*wmsk[1]
		wasz = sizfac*asz
	endif
	if Isnum(zfs) then begin
		list[2] = wmsk[2]
		wzfs = Cast(zfs,4)
	endif
	if Isnum(hsz) then begin
		list[3] = wmsk[3]
		nflg[3] = (hsz le 0)*wmsk[3]
		whsz = sizfac*hsz
	endif
	if Isnum(cln) then begin
		list[4] = wmsk[4]
		nflg[4] = (cln le 0)*wmsk[4]
		if keyword_set(rel) then begin
			if list[3] or not list[4] then wcln = whsz*cln $
			else message, 'Beam size must be provided when specifying /REL !'
		endif else wcln = sizfac*cln
	endif

	if not Arreq(nflg,bytarr(5)) then message, $
	'All inputs (except Z) must be positive!'
	if total(list) ne 3 then message, 'Wrong number of inputs, 3 required!'
	if list[2] then zsgn = 1 else zsgn = (-1)^keyword_set(cnv)
	whi = fix(total(list*2^indgen(5)))

	case whi of
		7	:	begin
					emitt = wssz*wasz
					if emitt ge wwavl then begin
						whsz = sqrt(wssz^2 + (wasz*wzfs)^2)
						wcln = wwavl*whsz/emitt
					endif else message, 'Emittance unacceptably small!'
				end
		11	:	begin
					emitt = wssz*wasz
					if emitt ge wwavl then begin
						tem = whsz^2 - wssz^2
						if tem ge 0 then begin
							wzfs = sqrt(tem)/wasz
							wcln = wwavl*whsz/emitt
						endif else message, 'Beam size unacceptably small!'
					endif else message, 'Emittance unacceptably small!'
				end
		13	:	begin
					if wzfs ne 0 then begin
						tem = whsz^2 - wssz^2
						if tem ge (wwavl*wzfs/wssz)^2 then begin
							wasz = sqrt(tem/wzfs^2)
							wcln = wwavl*whsz/(wssz*wasz)
						endif else message, 'Beam size unacceptably small!'
					endif else message, 'Ambiguous or impossible input!'
				end
		14	:	begin
					tem = whsz^2 - (wasz*wzfs)^2
					if tem ge (wwavl/wasz)^2 then begin
						wssz = sqrt(tem)
						wcln = wwavl*whsz/(wssz*wasz)
					endif else message, 'Beam size unacceptably small!'
				end
		19	:	begin
					emitt = wssz*wasz
					if emitt ge wwavl then begin
						tem = wcln^2 - (wwavl/wasz)^2
						if tem ge 0 then begin
							wzfs = sqrt(tem)/(wwavl/wssz)
							whsz = emitt*wcln/wwavl
						endif else message, 'Coher. length unacceptably small!'
					endif else message, 'Emittance unacceptably small!'
				end
		21	:	begin
					tem = wcln^2 - (wwavl*wzfs/wssz)^2
					if tem gt 0 and tem le wssz^2 then begin
						wasz = wwavl/sqrt(tem)
						whsz = wssz*wasz*wcln/wwavl
					endif else message, 'Unacceptable coherence length!'
				end
		22	:	begin
					if wzfs ne 0 then begin
						tem = wcln^2 - (wwavl/wasz)^2
						if tem gt 0 and tem le (wasz*wzfs)^2 then begin
							wssz = wwavl*sqrt(wzfs^2/tem)
							whsz = wssz*wasz*wcln/wwavl
						endif else message, 'Unacceptable coherence length!'
					endif else message, 'Ambiguous or impossible input!'
				end
		25	:	begin
					if whsz ge (wcln > wssz) then begin
						wasz = wwavl*whsz/(wssz*wcln)
						wzfs = sqrt(whsz^2 - wssz^2)/wasz
					endif else message, 'Unacceptable sizes!
				end
		26	:	begin
					if whsz ge wcln then begin
						tem = wcln^2 - (wwavl/wasz)^2
						if tem ge 0 then begin
							wssz = wwavl*whsz/(wasz*wcln)
							wzfs = sqrt(tem)*whsz/(wasz*wcln)
						endif else message, 'Coher. length unacceptably small!'
					endif else message, 'Beam size unacceptably small!'
				end
		28	:	begin
					if whsz ge wcln then begin
						tem = 1 - (2*wwavl*wzfs/(whsz*wcln))^2
						if tem ge 0 then begin
							stem = sqrt((1 + sqrt(tem))/2)
							fn = One_of(ner,far) > 0
							if fn then begin
								wssz = wwavl*abs(wzfs)/(wcln*stem)
								wasz = whsz*stem/abs(wzfs)
							endif else begin
								wssz = whsz*stem
								wasz = wwavl/(wcln*stem)
							endelse
						endif else message, 'Impossible sizes!'
					endif else message, 'Beam size unacceptably small!'
				end
		else:	message, 'What, what?'
	endcase

	ssz = wssz/sizfac
	asz = wasz/sizfac
	zfs = wzfs*zsgn
	hsz = whsz/sizfac
	cln = wcln/sizfac
	if keyword_set(rel) then cln = cln/hsz

	return
end