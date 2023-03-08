

	bw1 = 1.324e-4
	bw3 = 8.922e-6
	
	miran = 2.4e-3
	mirof = 3
	
	bethi = 2e-2*2.54
	
	kapel = ['h','c','n','o']
	kapwei = [10,22,2,5]
	kapden = 1.42
	kapthi = 15*2.54e-3
	
	airel = ['n','o','ar']
	airwei = [78,21,1]
	airden = 1.2e-3
	airthi = 150.
	
	ape = [2,1]
	dist=28
	
	rflu1 = $
Und_flux(ene,per=33,har=1,/qui,band=bw1,/def,/cor,/opt,ape=ape,dist=dist,set=sen)
	rflu3 = $
Und_flux(3*sen,per=33,har=3,/qui,band=bw3,/def,/cor,ape=ape,dist=dist)
	
	monref1 = Ref_curve(0,ene=ene,crys='si',ind=[1,1,1])
	monref3 = Ref_curve(0,ene=3*ene,crys='si',ind=[3,3,3])
	
	emonref1 = Ref_curve(0,ene=ene,crys='ge',ind=[1,1,1])
	emonref3 = Ref_curve(0,ene=3*ene,crys='ge',ind=[3,3,3])
	
	mirref1 = Mirror(ene,miran,ele='si',rough=mirof)
	mirref3 = Mirror(3*ene,miran,ele='si',rough=mirof)
	
	emirref1 = Mirror(ene,1e-3,ele='si',rough=mirof)
	emirref3 = Mirror(3*ene,1e-3,ele='si',rough=mirof)
	
	beabs1 = Abs_coeff(ene,elem='be')
	kapabs1 = Abs_coeff(ene,elem=kapel,wei=kapwei,/form,den=kapden)
	airabs1 = Abs_coeff(ene,elem=airel,wei=airwei,/form,den=airden)
	trans1 = exp(-bethi*beabs1-kapthi*kapabs1-airthi*airabs1)
	
	beabs3 = Abs_coeff(3*ene,elem='be')
	kapabs3 = Abs_coeff(3*ene,elem=kapel,wei=kapwei,/form,den=kapden)
	airabs3 = Abs_coeff(3*ene,elem=airel,wei=airwei,/form,den=airden)
	trans3 = exp(-bethi*beabs3-kapthi*kapabs3-airthi*airabs3)
	
	mfac1 = monref1^2*mirref1^2*trans1*emirref1*emonref1
	mfac3 = monref3^2*mirref3^2*trans3*emirref3*emonref3
	
	flu1 = rflu1*mfac1
	flu3 = rflu3*mfac3

