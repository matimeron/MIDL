Pro Foctest, ene, und_length = udl, foptloc = fop, soptloc = sop, samloc= sam,$
	f1 = f1, f2 = f2, horizontal = hor, vertical = ver, _extra = _e

	on_error, 1

	whi = abs(One_of(hor,ver))
	rsg = Und_beamsize(ene,und=udl,/def,ang=asg,_extra=_e)
	rsg = rsg[whi]
	asg = asg[whi]

	pqri = [rsg^2*asg^2,rsg^2,0*asg^2]
	pqr0 = PQR_prop(pqri,ene=ene,l=fop,rpsq=psq0,rqsq=qsq0,rr=r0)

	l1 = Cast(sop-fop,4)
	l2 = Cast(sam-sop,4)

	whi = One_of(f1,f2,val=wf) + 1
	case whi of
		0	:	message, 'Missing focal length input!'
		1	:	begin
					wf1 = wf
					pqr1 = PQR_prop(pqr0,ene=ene,f=wf1,l=l1,rqsq=qsq1,rr=r1)
					wf2 = 1./(r1/qsq1 + 1./l2)
					pqr2 = PQR_prop(pqr1,f=wf2,l=l2,rqsq=qsq2)
					lp = 1./(1./l1 + 1./l2 - 1./wf2)
				end
		2	:	begin
					wf2 = wf
					lp = 1./(1./l1 + 1./l2 - 1./wf2)
					wf1 = 1./(r0/qsq0 + 1./l1 - lp/l1^2)
					pqr1 = PQR_prop(pqr0,ene=ene,f=wf1,l=l1)
					pqr2 = PQR_prop(pqr1,f=wf2,l=l2,rqsq=qsq2)
				end
		else:	message, 'Something is wrong here!'
	endcase

	print
	print, l1, form = '("	L1	= ",f12.3," m")'
	print, l2, form = '("	L2	= ",f12.3," m")'
	print, lp, form = '("	LP	= ",f12.3," m")'
	print
	print, wf1, form = '("	F1	= ",f12.3," m")'
	print, wf2, form = '("	F2	= ",f12.3," m")'
	print
	print, 1e3*sqrt(alog(256)*qsq2), form = '("	FWHM	= ",f12.3," micron")'
	print

	return
end