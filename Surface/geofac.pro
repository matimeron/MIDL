Function Geofac, npoints = npn, alpha = alp, beta = bet, dtheta = dth, $
	s1 = s1, s2 = s2, s3 = s3, l2 = l2, l3 = l3, radians = rad, normalize = nrm

;+
; NAME:
;		GEOFAC
; VERSION:
;		4.8
; PURPOSE:
;		Calculating the "geometrical factor" caused by restricted field of view
;		of a beam footprint.
; CATEGORY:
;		X-ray calculations, surface specific.
; CALLING SEQUENCE:
;		Result = GEOFAC([NPOINTS = NPN,] ALPHA = ALP, [BETA= BET,] [DTH= DTH,]
;				L2 = L2, L3 = L3, S1 = S1, S2 = S2, S3 = S3 [,keywords])
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	NPOINTS
;		Two element numeric vector, number of integration points per quadrant.
;		The full number used (that's the number of rectangles into which the
;		detector slit is divided) is
;			[2*NPOINTS[0] + 1, 2*NPOINTS[1] + 1]
;		The default values are [0,0], giving a single evaluation point.  If a
;		single number is input, it is assigned to both X and Y.
;	ALPHA
;		The Alpha angle(s), in degrees unless /RADIANS is set.  Mandatory, no
;		default is provided.
;	BETA
;		The beta angle(s), in degrees unless /RADIANS is set.  Default is
;		Beta = Alpha.
;	DTHETA
;		The Detector_theta angle(s), in degrees unless /RADIANS is set.  Default
;		is zero.
;
;		Note:	Each of ALPHA, BETA and DTHETA can be either scalar or vector.
;				However, all the vector entries must be same length.
;
;	S1
;		A two-element vector, dimensions of the first (pre-sample) slit in X-Y
;		order.  Mandatory input, no defaults.
;	S2
;		A two-element vector, dimensions of the second (post-sample) slit in X-Y
;		order.  Mandatory input, no defaults.
;	S3
;		A two-element vector, dimensions of the third (pre-detector) slit in X-Y
;		order.  Mandatory input, no defaults.
;	L2
;		Scalar input, the distance from sample to S2.  No defaults.
;	L3
;		Scalar input, the distance from S2 to S3.  No defaults.
;
;		Note: 	S1, S2, S3, L2 and L3 must be in same units (whatever they are).
;	/RADIANS
;		Switch.  If set the angles are assumed to be given in radians.
;	/NORMALIZE
;		Switch.  If set, the results for each point are normalized to those for
;		same ALPHA, BETA = Alpha and DTHETA = 0.
; OUTPUTS:
;		Returns the values of the geometric factor, normalized if /NORMALIZE is
;		set.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls ARREQ, DEFAULT, SHAPE_AREA and SHAPE_OVERLAP,
;		from MIDL.  Calls itself recursively.
; MODIFICATION HISTORY:
;		Created 20-MAR-2004 by Mati Meron.
;-

	on_error, 1

	case n_elements(npn) of
		0	:	wnpn = [0l,0l]
		1	:	wnpn = long([npn,npn])
		2	:	wnpn = long(npn)
		else:	message, 'NPOINTS can have only < 2 elements!'
	endcase

	if keyword_set(rad) then mult = 1. else mult = !dtor
	bet = Default(bet,alp)
	dth = Default(dth,0.)
	nums = [n_elements(alp), n_elements(bet), n_elements(dth)]
	nmx = max(nums)
	dum = where(nums lt nmx and nums ne 1, ndum)
	if ndum gt 0 then message, 'Inconsistent data lengths!'
	snums = [n_elements(s1),n_elements(s2),n_elements(s3)]
	if not Arreq(snums,[2,2,2]) then message, 'Bad slit data!'
	lnums = [n_elements(l2),n_elements(l3)]
	if not Arreq(lnums,[1,1]) then message, 'Bad distances data!'

	if nums[0] eq nmx then walp = mult*alp else walp = mult*replicate(alp,nmx)
	if nums[1] eq nmx then wbet = mult*bet else wbet = mult*replicate(bet,nmx)
	if nums[2] eq nmx then wdth = mult*dth else wdth = mult*replicate(dth,nmx)

	sina = sin(walp)
	sinb = sin(wbet)
	sind = sin(wdth)
	cosd = cos(wdth)

	ibeam = fltarr(2,nmx)
	ibeam[0,*] = 0.5*s1[0]
	ibeam[1,*] = 0.5*s1[1]/sina

	trmat = fltarr(2,2,nmx)
	trmat[0,0,*] = cosd
	trmat[0,1,*] = sind
	trmat[1,0,*] = -sinb*sind
	trmat[1,1,*] = sinb*cosd

	lrat = 1.*l2/l3
	tem = 1./2*s2
	scor = (1 + lrat)*[[tem],[tem*[-1,1]],[tem*[-1,-1]],[tem*[1,-1]]]
	del = s3/(2.*wnpn + 1)
	arel = del[0]*del[1]
	del = lrat*del
	dfov = fltarr([2,4,wnpn+1])
	wei = fltarr(wnpn+1)
	for i = 0l,wnpn[0] do begin
		for j = 0, wnpn[1] do begin
			rd = [i,j]*del
			dcor = [[rd],[rd],[rd],[rd]]
			dfov[*,*,i,j] = scor - dcor
			wei[i,j] = 1 + ((i+j) gt 0) + 2*((i*j) gt 0)
		endfor
	endfor

	res = fltarr(nmx)
	dar = fltarr(wnpn+1)
	for i = 0l, nmx - 1 do begin
		tem = ibeam[*,i]
		ftp = [[tem],[tem*[-1,1]],[tem*[-1,-1]],[tem*[1,-1]]]
		samp = reform(trmat[*,*,i])#ftp
		for j = 0l, wnpn[0] do for k = 0l, wnpn[1] do $
		dar[j,k] = Shape_area(Shape_overlap(samp,dfov[*,*,j,k]))
		res[i] = arel*total(dar*wei)
	endfor
	res = res*sina/sinb
	if keyword_set(nrm) then res = $
	res/Geofac(alp=alp,npoints=wnpn,l2=l2,l3=l3,s1=s1,s2=s2,s3=s3)

	if nmx eq 1 then return, res[0] else return, res
end