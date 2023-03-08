Function NFoctest, ene, und_length=udl, foptloc=fop, soptloc=sop, samloc=sam, $
	f1 = f1, f2 = f2, horizontal = hor, vertical = ver, fwhm = fwhm, $
	ltil = lp, ffocloc = ffl, rf1 = wf1, rf2 = wf2, _extra = _e

	on_error, 1

	l1 = Cast(sop-fop,4)
	l2 = Cast(sam-sop,4)

	whi = abs(One_of(hor,ver))
	rsg = Und_beamsize(ene,und=udl,/def,ang=asg,_extra=_e)
	rsg = rsg[whi]
	asg = asg[whi]
	pqri = [rsg^2*asg^2,rsg^2,0*asg^2]

	wha = One_of(f1,f2,val=wf) + 1
	if wha gt 0 then begin
		nf = n_elements(wf)
		tpqr = fltarr(3,nf)
		for i = 0, 2 do tpqr[i,*] = replicate(pqri[i],nf)
		pqri = tpqr
	endif else message, 'Missing focal length input!'

	pqr0 = PQR_prop(pqri,ene=ene,l=fop,rpsq=psq0,rqsq=qsq0,rr=r0)

	case wha of
		1	:	begin
					wf1 = Cast(wf,4)
					pqr1 = PQR_prop(pqr0,ene=ene,f=wf1,l=l1,rqsq=qsq1,rr=r1)
					wf2 = 1./(r1/qsq1 + 1./l2)
					pqr2 = PQR_prop(pqr1,f=wf2,l=l2,rqsq=qsq2)
					lp = 1./(1./l1 + 1./l2 - 1./wf2)
				end
		2	:	begin
					wf2 = Cast(wf,4)
					lp = 1./(1./l1 + 1./l2 - 1./wf2)
					wf1 = 1./(r0/qsq0 + 1./l1 - lp/l1^2)
					pqr1 = PQR_prop(pqr0,ene=ene,f=wf1,l=l1)
					pqr2 = PQR_prop(pqr1,f=wf2,l=l2,rqsq=qsq2)
				end
		else:	message, 'Something is wrong here!'
	endcase

	ffl = 1./(1/wf1 - r0/qsq0) + fop
	res = 1e3*sqrt(qsq2)
	if keyword_set(fwhm) then res = sqrt(alog(256))*res

	return, res
end