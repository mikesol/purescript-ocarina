var Ob=function(t){return function(e){for(var r=e.length,n=new Array(r),a=0;a<r;a++)n[a]=t(e[a]);return n}};var Yo={compose:function(t){return function(e){return function(r){return t(e(r))}}}},ho=function(t){return t.compose};var Z=function(t){return t.identity},et={identity:function(t){return t},Semigroupoid0:function(){return Yo}};var Ze=!0;var Tt=function(t){return function(e){return function(r){return t(r)(e)}}},T=function(t){return function(e){return t}};var zf=function(t){return function(e){return e(t)}},tl=function(t){return function(e){return t(e)}};var d=function(){function t(){}return t.value=new t,t}();var p=function(t){return t.map},tr=function(t){return function(e){return function(r){return p(t)(r)(e)}}},Kt=function(t){return p(t)(T(void 0))},X=function(t){return function(e){return function(r){return p(t)(T(r))(e)}}},L_=function(t){return function(e){return p(t)(T(e))}};var Ia={map:ho(Yo)},xe={map:Ob},js=function(t){return function(e){return function(r){return p(t)(function(n){return n(r)})(e)}}};var Ib=function(t){return function(e){return t.length===0?e:e.length===0?t:t.concat(e)}};var Er=function(t){return t.reflectSymbol};var W_=function(t){return function(e){return{}.hasOwnProperty.call(e,t)}},Ja=function(t){return function(e){return e[t]}},Ju=function(t){return function(e){return function(r){var n={};for(var a in r)({}).hasOwnProperty.call(r,a)&&(n[a]=r[a]);return n[t]=e,n}}};var Nb={append:function(t){return function(e){return void 0}}};var Ra={append:Ib};var gt=function(t){return t.append},Qs=function(t){return{append:function(e){return function(r){return function(n){return gt(t)(e(n))(r(n))}}}}};var O=function(t){return t.alt};var Lb=function(t){return function(e){for(var r=t.length,n=e.length,a=new Array(r*n),u=0,f=0;f<r;f++)for(var i=t[f],m=0;m<n;m++)a[u++]=i(e[m]);return a}};var el={apply:Lb,Functor0:function(){return xe}},zt=function(t){return t.apply};var j=function(t){return function(e){return function(r){return zt(t)(p(t.Functor0())(T(Z(et)))(e))(r)}}},ca=function(t){return function(e){return function(r){return function(n){return zt(t)(p(t.Functor0())(e)(r))(n)}}}};var h=function(t){return t.pure};var $n=function(t){return function(e){return function(r){if(e)return r;if(!e)return h(t)(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[e.constructor.name,r.constructor.name])}}},rl=function(t){return function(e){return function(r){return zt(t.Apply0())(h(t)(e))(r)}}};var lr={pure:function(t){return[t]},Apply0:function(){return el}};var Wb=function(t){return function(e){for(var r=[],n=0,a=t.length;n<a;n++)Array.prototype.push.apply(r,e(t[n]));return r}};var Gr=function(t){return t.discard};var Zo={bind:Wb,Apply0:function(){return el}},q=function(t){return t.bind},Yn=function(t){return Tt(q(t))};var Vf=function(t){return function(e){return function(r){return function(n){return q(t)(e(n))(r)}}}};var Sr={discard:function(t){return q(t)}};var Zn=function(t){return function(e){return q(t)(e)(Z(et))}};var ft=function(t){return t};var Gb=ft;var Bb=function(t){return function(e){return function(){return t(e())}}};function Br(t){return function(){return{value:t}}}var $r=function(t){return function(){return t.value}},Ub=function(t){return function(e){return function(){var r=t(e.value);return e.value=r.state,r.value}}},an=function(t){return function(e){return function(){return e.value=t}}};var ju=function(t){return function(e){return function(r){return q(t.Bind1())(e)(function(n){return q(t.Bind1())(r)(function(a){return h(t.Applicative0())(n(a))})})}}};var v0=String.fromCharCode(65535),D0=String.fromCharCode(0),d0=Number.POSITIVE_INFINITY,b0=Number.NEGATIVE_INFINITY;var qb=function(t){return function(e){return function(r){return function(n){return function(a){return n<a?t:n===a?e:r}}}}};var Hb=qb,zb=qb;var Vb=function(t){return function(e){return t===e}};var Jb=Vb,jb=Vb;var nl={eq:jb},Mi={eq:Jb};var ce=function(t){return t.eq};var ne=function(){function t(){}return t.value=new t,t}(),be=function(){function t(){}return t.value=new t,t}(),Ae=function(){function t(){}return t.value=new t,t}();var Xb=function(t){return function(e){return t-e|0}},Qb=function(t){return function(e){return t-e}};var Kb=function(t){return function(e){return t+e|0}},Yb=function(t){return function(e){return t*e|0}},Zb=function(t){return function(e){return t+e}},tA=function(t){return function(e){return t*e}};var ya=function(t){return t.zero};var ka={add:Zb,zero:0,mul:tA,one:1},xu={add:Kb,zero:0,mul:Yb,one:1};var ga=function(t){return t.one};var Wn=function(t){return t.mul};var Le=function(t){return t.add};var Fu=function(t){return t.sub};var of={sub:Qb,Semiring0:function(){return ka}},Zs={sub:Xb,Semiring0:function(){return xu}};var al=function(t){return function(e){return Fu(t)(ya(t.Semiring0()))(e)}};var Na=function(){return{compare:zb(ne.value)(Ae.value)(be.value),Eq0:function(){return nl}}}(),jr=function(){return{compare:Hb(ne.value)(Ae.value)(be.value),Eq0:function(){return Mi}}}();var ee=function(t){return t.compare};var eA=function(t){return function(e){return function(r){var n=ee(t)(e)(r);return!(n instanceof ne)}}};var $u=function(t){return function(e){return function(r){var n=ee(t)(e)(r);if(n instanceof ne)return r;if(n instanceof Ae||n instanceof be)return e;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[n.constructor.name])}}};var rm=function(t){return function(e){return function(r){var n=eA(t)(r)(ya(e.Semiring0()));return n?r:al(e)(r)}}};var Gn=function(t){return t.top};var cf={top:2147483647,bottom:-2147483648,Ord0:function(){return jr}};var Bn=function(t){return t.bottom};var nA=function(t){return t.toString()},aA=function(t){var e=t.toString();return isNaN(e+".0")?e:e+".0"};var B_={show:aA},ja={show:nA};var Xt=function(t){return t.show};var V=function(){function t(){}return t.value=new t,t}(),B=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var Ur=function(t){return function(e){return function(r){if(r instanceof V)return t;if(r instanceof B)return e(r.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}};var Mr={map:function(t){return function(e){return e instanceof B?new B(t(e.value0)):V.value}}};var Ca=function(t){return Ur(t)(Z(et))},ta=function(){return function(t){if(t instanceof B)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var Pi={apply:function(t){return function(e){if(t instanceof B)return p(Mr)(t.value0)(e);if(t instanceof V)return V.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,e.constructor.name])}},Functor0:function(){return Mr}},Ea={bind:function(t){return function(e){if(t instanceof B)return e(t.value0);if(t instanceof V)return V.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,e.constructor.name])}},Apply0:function(){return Pi}};var xo=function(){return{pure:B.create,Apply0:function(){return Pi}}}();var Yt=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Zt=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var _f={map:function(t){return function(e){if(e instanceof Yt)return new Yt(e.value0);if(e instanceof Zt)return new Zt(t(e.value0));throw new Error("Failed pattern match at Data.Either (line 31, column 1 - line 31, column 52): "+[e.constructor.name])}}};var La=function(t){return function(e){return function(r){if(r instanceof Yt)return t(r.value0);if(r instanceof Zt)return e(r.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},U_=function(){return La(T(V.value))(B.create)}();var ou=function(t){return t};var Fo={map:function(t){return function(e){return t(e)}}};var uA={apply:function(t){return function(e){return t(e)}},Functor0:function(){return Fo}},R0={bind:function(t){return function(e){return e(t)}},Apply0:function(){return uA}},um={pure:ou,Apply0:function(){return uA}},Qu={Applicative0:function(){return um},Bind1:function(){return R0}};var oA=function(t){return Math.min(Math.abs(t),2147483647)},iA=function(t){return function(e){return e===0?0:e>0?Math.floor(t/e):-Math.floor(t/-e)}},fA=function(t){return function(e){if(e===0)return 0;var r=Math.abs(e);return(t%r+r)%r}},cA=function(t){return function(e){return t/e}};var lA={Ring0:function(){return of}},_A={Ring0:function(){return Zs}};var iu=function(t){return t.mod};var cl={degree:function(t){return 1},div:cA,mod:function(t){return function(e){return 0}},CommutativeRing0:function(){return lA}},$o={degree:oA,div:iA,mod:fA,CommutativeRing0:function(){return _A}},Ku=function(t){return t.div};var un={mempty:void 0,Semigroup0:function(){return Nb}};var Pt=function(t){return t.mempty},Xr=function(t){return{mempty:function(e){return Pt(t)},Semigroup0:function(){return Qs(t.Semigroup0())}}};var om=function(t){return function(){return t}},pA=function(t){return function(e){return function(){return e(t())()}}};var ll=function(t){return function(e){return function(){for(var r=0,n=t.length;r<n;r++)e(t[r])()}}};var sA=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}},ue={Applicative0:function(){return c},Bind1:function(){return hr}},hr={bind:pA,Apply0:function(){return im(0)}},c={pure:om,Apply0:function(){return im(0)}},mA=sA("functorEffect","Effect",function(){return{map:rl(c)}}),im=sA("applyEffect","Effect",function(){return{apply:ju(ue),Functor0:function(){return mA(0)}}}),x=mA(20),tt=im(23),vA=function(t){return{append:ca(tt)(gt(t))}},Qr=function(t){return{mempty:om(Pt(t)),Semigroup0:function(){return vA(t.Semigroup0())}}};var DA=function(t){return function(){return{value:t}}};var wr=function(t){return function(){return t.value}},dA=function(t){return function(e){return function(){var r=t(e.value);return e.value=r.state,r.value}}},Mn=function(t){return function(e){return function(){e.value=t}}};var _r=DA,U0=dA,jf=function(t){return U0(function(e){var r=t(e);return{state:r,value:r}})},pf=function(t){return function(e){return Kt(x)(jf(t)(e))}};var J0=Ub,Sa=function(t){return J0(function(e){var r=t(e);return{state:r,value:r}})},Oi={map:Bb};var o={liftST:Gb,Monad0:function(){return ue}},Rt=function(t){return t.liftST};var dn=function(t){return function(e){for(var r=t>e?-1:1,n=new Array(r*(e-t)+1),a=t,u=0;a!==e;)n[u++]=a,a+=r;return n[u]=a,n}},X0=function(t){return function(e){if(t<1)return[];var r=new Array(t);return r.fill(e)}},Q0=function(t){return function(e){for(var r=[],n=0,a=0;a<t;a++)r[n++]=e;return r}},q_=typeof Array.prototype.fill=="function"?X0:Q0,K0=function(){function t(a,u){this.head=a,this.tail=u}var e={};function r(a){return function(u){return new t(a,u)}}function n(a){for(var u=[],f=0,i=a;i!==e;)u[f++]=i.head,i=i.tail;return u}return function(a){return function(u){return n(a(r)(e)(u))}}}(),Mu=function(t){return t.length};var AA=function(t){return function(e){return function(r){return function(n){for(var a=0,u=n.length;a<u;a++)if(r(n[a]))return t(a);return e}}}};var yA=function(t){return function(e){return function(r){return function(n){if(r<0||r>=n.length)return e;var a=n.slice();return a.splice(r,1),t(a)}}}};var Y0=function(){function t(e,r,n,a,u,f){var i,m,s,_,v,D,l;for(i=u+(f-u>>1),i-u>1&&t(e,r,a,n,u,i),f-i>1&&t(e,r,a,n,i,f),m=u,s=i,_=u;m<i&&s<f;)v=a[m],D=a[s],l=r(e(v)(D)),l>0?(n[_++]=D,++s):(n[_++]=v,++m);for(;m<i;)n[_++]=a[m++];for(;s<f;)n[_++]=a[s++]}return function(e){return function(r){return function(n){var a;return n.length<2?n:(a=n.slice(0),t(e,r,a,n.slice(0),0,n.length),a)}}}}();var _l=function(t){return function(e){return function(r){for(var n=e.length<r.length?e.length:r.length,a=new Array(n),u=0;u<n;u++)a[u]=t(e[u])(r[u]);return a}}};var kA=function(t){return function(e){return t[e]}};var tT=function(){function t(e,r,n,a,u,f){var i,m,s,_,v,D,l;for(i=u+(f-u>>1),i-u>1&&t(e,r,a,n,u,i),f-i>1&&t(e,r,a,n,i,f),m=u,s=i,_=u;m<i&&s<f;)v=a[m],D=a[s],l=r(e(v)(D)),l>0?(n[_++]=D,++s):(n[_++]=v,++m);for(;m<i;)n[_++]=a[m++];for(;s<f;)n[_++]=a[s++]}return function(e){return function(r){return function(n){return function(){return n.length<2||t(e,r,n,n.slice(0),0,n.length),n}}}}}();var hA=function(t){return function(e){return t&&e}},TA=function(t){return function(e){return t||e}},xA=function(t){return!t};var lu=function(t){return t.not};var Qf=function(t){return t.disj},Ga={ff:!1,tt:!0,implies:function(t){return function(e){return Qf(Ga)(lu(Ga)(t))(e)}},conj:hA,disj:TA,not:xA};var $A=function(t){return function(e){return function(r){for(var n=e,a=r.length,u=a-1;u>=0;u--)n=t(r[u])(n);return n}}},MA=function(t){return function(e){return function(r){for(var n=e,a=r.length,u=0;u<a;u++)n=t(n)(r[u]);return n}}};var $=function(t){return t.empty};var nt=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}(),Kf=function(t){return function(e){return t(e.value0)(e.value1)}};var on=function(t){return t.value1};var ti={map:function(t){return function(e){return new nt(e.value0,t(e.value1))}}};var Ua=function(t){return t.value0};var wu=function(){return ft};var fn=wu,wn=wu;var Dm=function(){return function(){return function(t){return wu()}}};var rr=function(t){return t.foldr};var Ve=function(t){return function(e){return rr(t)(O(e.Alt0()))($(e))}},Nr=function(t){return function(e){return function(r){return rr(t)(function(){var n=O(e.Alt0());return function(a){return n(r(a))}}())($(e))}}},nr=function(t){return function(e){return function(r){return rr(e)(function(){var n=j(t.Apply0());return function(a){return n(r(a))}}())(h(t)(void 0))}}},ea=function(t){return function(e){return Tt(nr(t)(e))}},K_=function(t){return function(e){return nr(t)(e)(Z(et))}},ar=function(t){return t.foldl};var Je={foldr:function(t){return function(e){return function(r){if(r instanceof V)return e;if(r instanceof B)return t(r.value0)(e);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},foldl:function(t){return function(e){return function(r){if(r instanceof V)return e;if(r instanceof B)return t(e)(r.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},foldMap:function(t){return function(e){return function(r){if(r instanceof V)return Pt(t);if(r instanceof B)return e(r.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[e.constructor.name,r.constructor.name])}}}};var RA=function(t){return function(e){return function(r){return rr(t)(function(n){return function(a){return gt(e.Semigroup0())(r(n))(a)}})(Pt(e))}}},wt={foldr:$A,foldl:MA,foldMap:function(t){return RA(wt)(t)}},Y_=function(t){return function(e){return function(r){return ar(t)(function(n){return function(a){return gt(e.Semigroup0())(n)(r(a))}})(Pt(e))}}},qn=function(t){return t.foldMap};var NA=function(){function t(a){return[a]}function e(a){return function(u){return[a,u]}}function r(a){return function(u){return function(f){return[a,u,f]}}}function n(a){return function(u){return a.concat(u)}}return function(a){return function(u){return function(f){return function(i){return function(m){function s(_,v){switch(v-_){case 0:return f([]);case 1:return u(t)(i(m[_]));case 2:return a(u(e)(i(m[_])))(i(m[_+1]));case 3:return a(a(u(r)(i(m[_])))(i(m[_+1])))(i(m[_+2]));default:var D=_+Math.floor((v-_)/4)*2;return a(u(n)(s(_,D)))(s(D,v))}}return s(0,m.length)}}}}}}();var ra=function(t){return t.traverse};var VA=function(t){return function(e){return ra(t)(e)(Z(et))}},ei={traverse:function(t){return NA(zt(t.Apply0()))(p(t.Apply0().Functor0()))(h(t))},sequence:function(t){return VA(ei)(t)},Functor0:function(){return xe},Foldable1:function(){return wt}};var kl=function(){return _l(nt.create)}();var Fm=function(){return kA};var ZA=function(t){return[t]};var ty=function(){return AA(B.create)(V.value)}();var $m=function(){return yA(B.create)(V.value)}(),Mm=function(t){return function(e){return function(r){return r.length===0?[]:Ur(r)(function(n){return ta()($m(n)(r))})(ty(t(e))(r))}}};var Ni=function(t){return function(e){return gt(Ra)([t])(e)}};var ey=function(t){return function(e){for(var r=e.length,n=Array(r),a=0;a<r;a++)n[a]=t(a)(e[a]);return n}};var ro=function(t){return t.mapWithIndex};var ni={mapWithIndex:ey,Functor0:function(){return xe}};var wo=function(t){return t.foldrWithIndex};var ao=function(t){return t.foldlWithIndex};var ai=function(t){return t.foldMapWithIndex};var Li=function(t){return t.traverseWithIndex};var uo=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}();var ip=function(t){return function(e){return new uo(e,$(t))}};var Or=function(){function t(){}return t.value=new t,t}(),se=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}(),fp=function(t){return t},$x=function(t){return new se(t.value0,t.value1)};var Mx=function(t){var e=function(r){return function(n){var a=r,u=!1,f;function i(m,s){if(s instanceof se&&s.value1 instanceof se&&s.value1.value1 instanceof se){a=new se(s,m),n=s.value1.value1.value1;return}var _=function(D){return D instanceof se&&D.value1 instanceof se&&D.value1.value1 instanceof Or?new se(t(D.value0),new se(t(D.value1.value0),Or.value)):D instanceof se&&D.value1 instanceof Or?new se(t(D.value0),Or.value):Or.value},v=function(D){return function(l){var C=D,ot=!1,vt;function Gt(re,he){if(re instanceof se&&re.value0 instanceof se&&re.value0.value1 instanceof se&&re.value0.value1.value1 instanceof se){C=re.value1,l=new se(t(re.value0.value0),new se(t(re.value0.value1.value0),new se(t(re.value0.value1.value1.value0),he)));return}return ot=!0,he}for(;!ot;)vt=Gt(C,l);return vt}};return u=!0,v(m)(_(s))}for(;!u;)f=i(a,n);return f}};return e(Or.value)},cp={map:Mx};var qa={foldr:function(t){return function(e){var r=function(){var a=function(u){return function(f){var i=u,m=!1,s;function _(v,D){if(D instanceof Or)return m=!0,v;if(D instanceof se){i=new se(D.value0,v),f=D.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[v.constructor.name,D.constructor.name])}for(;!m;)s=_(i,f);return s}};return a(Or.value)}(),n=ar(qa)(Tt(t))(e);return function(a){return n(r(a))}}},foldl:function(t){var e=function(r){return function(n){var a=r,u=!1,f;function i(m,s){if(s instanceof Or)return u=!0,m;if(s instanceof se){a=t(m)(s.value0),n=s.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[s.constructor.name])}for(;!u;)f=i(a,n);return f}};return e},foldMap:function(t){return function(e){return ar(qa)(function(r){var n=gt(t.Semigroup0())(r);return function(a){return n(e(a))}})(Pt(t))}}};var gl={append:function(t){return function(e){return rr(qa)(se.create)(e)(t)}}};var Pm={append:function(t){return function(e){return new uo(t.value0,gt(gl)(t.value1)($x(e)))}}};var uy={alt:gt(gl),Functor0:function(){return cp}},Om=function(){return{empty:Or.value,Alt0:function(){return uy}}}();var _y=function(t){return t()};var py=function(t){throw new Error(t)};var sy=function(){return py};var Zx=_y,pu=function(t){return Zx(function(){return sy()(t)})};var Qt=function(){function t(){}return t.value=new t,t}(),ve=function(){function t(e,r,n,a){this.value0=e,this.value1=r,this.value2=n,this.value3=a}return t.create=function(e){return function(r){return function(n){return function(a){return new t(e,r,n,a)}}}},t}(),We=function(){function t(e,r,n,a,u,f,i){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=f,this.value6=i}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return new t(e,r,n,a,u,f,i)}}}}}}},t}(),Wi=function(){function t(e,r,n){this.value0=e,this.value1=r,this.value2=n}return t.create=function(e){return function(r){return function(n){return new t(e,r,n)}}},t}(),fi=function(){function t(e,r,n){this.value0=e,this.value1=r,this.value2=n}return t.create=function(e){return function(r){return function(n){return new t(e,r,n)}}},t}(),Gi=function(){function t(e,r,n,a,u,f){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=f}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return new t(e,r,n,a,u,f)}}}}}},t}(),Oo=function(){function t(e,r,n,a,u,f){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=f}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return new t(e,r,n,a,u,f)}}}}}},t}(),Bi=function(){function t(e,r,n,a,u,f){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=f}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return new t(e,r,n,a,u,f)}}}}}},t}(),_p=function(){function t(e,r,n,a){this.value0=e,this.value1=r,this.value2=n,this.value3=a}return t.create=function(e){return function(r){return function(n){return function(a){return new t(e,r,n,a)}}}},t}();var vy=function(t){return function(e){return new ve(Qt.value,t,e,Qt.value)}};var uF=function(t){return function(e){var r=ee(t),n=function(a){var u=!1,f;function i(m){if(m instanceof Qt)return u=!0,V.value;if(m instanceof ve){var s=r(e)(m.value1);if(s instanceof Ae)return u=!0,new B(m.value2);if(s instanceof ne){a=m.value0;return}a=m.value3;return}if(m instanceof We){var _=r(e)(m.value1);if(_ instanceof Ae)return u=!0,new B(m.value2);var v=r(e)(m.value4);if(v instanceof Ae)return u=!0,new B(m.value5);if(_ instanceof ne){a=m.value0;return}if(v instanceof be){a=m.value6;return}a=m.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[m.constructor.name])}for(;!u;)f=i(a);return f};return n}};var Dy=function(t){return t instanceof Qt};var cn=function(t){return function(e){return function(r){var n=t,a=e,u=!1,f;function i(m,s,_){if(s instanceof Or)return u=!0,_;if(s instanceof se){if(s.value0 instanceof Wi){n=m,a=s.value1,r=new ve(_,s.value0.value0,s.value0.value1,s.value0.value2);return}if(s.value0 instanceof fi){n=m,a=s.value1,r=new ve(s.value0.value0,s.value0.value1,s.value0.value2,_);return}if(s.value0 instanceof Gi){n=m,a=s.value1,r=new We(_,s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof Oo){n=m,a=s.value1,r=new We(s.value0.value0,s.value0.value1,s.value0.value2,_,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof Bi){n=m,a=s.value1,r=new We(s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5,_);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[s.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[s.constructor.name,_.constructor.name])}for(;!u;)f=i(n,a,r);return f}}},Sl=function(t){return function(e){return function(r){var n=function(f){return function(i){var m=f,s=!1,_;function v(D,l){if(D instanceof Or)return s=!0,new ve(l.value0,l.value1,l.value2,l.value3);if(D instanceof se){if(D.value0 instanceof Wi)return s=!0,cn(t)(D.value1)(new We(l.value0,l.value1,l.value2,l.value3,D.value0.value0,D.value0.value1,D.value0.value2));if(D.value0 instanceof fi)return s=!0,cn(t)(D.value1)(new We(D.value0.value0,D.value0.value1,D.value0.value2,l.value0,l.value1,l.value2,l.value3));if(D.value0 instanceof Gi){m=D.value1,i=new _p(new ve(l.value0,l.value1,l.value2,l.value3),D.value0.value0,D.value0.value1,new ve(D.value0.value2,D.value0.value3,D.value0.value4,D.value0.value5));return}if(D.value0 instanceof Oo){m=D.value1,i=new _p(new ve(D.value0.value0,D.value0.value1,D.value0.value2,l.value0),l.value1,l.value2,new ve(l.value3,D.value0.value3,D.value0.value4,D.value0.value5));return}if(D.value0 instanceof Bi){m=D.value1,i=new _p(new ve(D.value0.value0,D.value0.value1,D.value0.value2,D.value0.value3),D.value0.value4,D.value0.value5,new ve(l.value0,l.value1,l.value2,l.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[D.value0.constructor.name,l.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[D.constructor.name,l.constructor.name])}for(;!s;)_=v(m,i);return _}},a=ee(t),u=function(f){return function(i){var m=f,s=!1,_;function v(D,l){if(l instanceof Qt)return s=!0,n(D)(new _p(Qt.value,e,r,Qt.value));if(l instanceof ve){var C=a(e)(l.value1);if(C instanceof Ae)return s=!0,cn(t)(D)(new ve(l.value0,e,r,l.value3));if(C instanceof ne){m=new se(new Wi(l.value1,l.value2,l.value3),D),i=l.value0;return}m=new se(new fi(l.value0,l.value1,l.value2),D),i=l.value3;return}if(l instanceof We){var ot=a(e)(l.value1);if(ot instanceof Ae)return s=!0,cn(t)(D)(new We(l.value0,e,r,l.value3,l.value4,l.value5,l.value6));var vt=a(e)(l.value4);if(vt instanceof Ae)return s=!0,cn(t)(D)(new We(l.value0,l.value1,l.value2,l.value3,e,r,l.value6));if(ot instanceof ne){m=new se(new Gi(l.value1,l.value2,l.value3,l.value4,l.value5,l.value6),D),i=l.value0;return}if(ot instanceof be&&vt instanceof ne){m=new se(new Oo(l.value0,l.value1,l.value2,l.value4,l.value5,l.value6),D),i=l.value3;return}m=new se(new Bi(l.value0,l.value1,l.value2,l.value3,l.value4,l.value5),D),i=l.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[D.constructor.name,l.constructor.name])}for(;!s;)_=v(m,i);return _}};return u(Or.value)}}},oF=function(t){return function(e){var r=function(i){return function(m){var s=i,_=!1,v;function D(l,C){if(l instanceof Or)return _=!0,C;if(l instanceof se){if(l.value0 instanceof Wi&&l.value0.value2 instanceof Qt&&C instanceof Qt)return _=!0,cn(t)(l.value1)(new ve(Qt.value,l.value0.value0,l.value0.value1,Qt.value));if(l.value0 instanceof fi&&l.value0.value0 instanceof Qt&&C instanceof Qt)return _=!0,cn(t)(l.value1)(new ve(Qt.value,l.value0.value1,l.value0.value2,Qt.value));if(l.value0 instanceof Wi&&l.value0.value2 instanceof ve){s=l.value1,m=new We(C,l.value0.value0,l.value0.value1,l.value0.value2.value0,l.value0.value2.value1,l.value0.value2.value2,l.value0.value2.value3);return}if(l.value0 instanceof fi&&l.value0.value0 instanceof ve){s=l.value1,m=new We(l.value0.value0.value0,l.value0.value0.value1,l.value0.value0.value2,l.value0.value0.value3,l.value0.value1,l.value0.value2,C);return}return l.value0 instanceof Wi&&l.value0.value2 instanceof We?(_=!0,cn(t)(l.value1)(new ve(new ve(C,l.value0.value0,l.value0.value1,l.value0.value2.value0),l.value0.value2.value1,l.value0.value2.value2,new ve(l.value0.value2.value3,l.value0.value2.value4,l.value0.value2.value5,l.value0.value2.value6)))):l.value0 instanceof fi&&l.value0.value0 instanceof We?(_=!0,cn(t)(l.value1)(new ve(new ve(l.value0.value0.value0,l.value0.value0.value1,l.value0.value0.value2,l.value0.value0.value3),l.value0.value0.value4,l.value0.value0.value5,new ve(l.value0.value0.value6,l.value0.value1,l.value0.value2,C)))):l.value0 instanceof Gi&&l.value0.value2 instanceof Qt&&l.value0.value5 instanceof Qt&&C instanceof Qt?(_=!0,cn(t)(l.value1)(new We(Qt.value,l.value0.value0,l.value0.value1,Qt.value,l.value0.value3,l.value0.value4,Qt.value))):l.value0 instanceof Oo&&l.value0.value0 instanceof Qt&&l.value0.value5 instanceof Qt&&C instanceof Qt?(_=!0,cn(t)(l.value1)(new We(Qt.value,l.value0.value1,l.value0.value2,Qt.value,l.value0.value3,l.value0.value4,Qt.value))):l.value0 instanceof Bi&&l.value0.value0 instanceof Qt&&l.value0.value3 instanceof Qt&&C instanceof Qt?(_=!0,cn(t)(l.value1)(new We(Qt.value,l.value0.value1,l.value0.value2,Qt.value,l.value0.value4,l.value0.value5,Qt.value))):l.value0 instanceof Gi&&l.value0.value2 instanceof ve?(_=!0,cn(t)(l.value1)(new ve(new We(C,l.value0.value0,l.value0.value1,l.value0.value2.value0,l.value0.value2.value1,l.value0.value2.value2,l.value0.value2.value3),l.value0.value3,l.value0.value4,l.value0.value5))):l.value0 instanceof Oo&&l.value0.value0 instanceof ve?(_=!0,cn(t)(l.value1)(new ve(new We(l.value0.value0.value0,l.value0.value0.value1,l.value0.value0.value2,l.value0.value0.value3,l.value0.value1,l.value0.value2,C),l.value0.value3,l.value0.value4,l.value0.value5))):l.value0 instanceof Oo&&l.value0.value5 instanceof ve?(_=!0,cn(t)(l.value1)(new ve(l.value0.value0,l.value0.value1,l.value0.value2,new We(C,l.value0.value3,l.value0.value4,l.value0.value5.value0,l.value0.value5.value1,l.value0.value5.value2,l.value0.value5.value3)))):l.value0 instanceof Bi&&l.value0.value3 instanceof ve?(_=!0,cn(t)(l.value1)(new ve(l.value0.value0,l.value0.value1,l.value0.value2,new We(l.value0.value3.value0,l.value0.value3.value1,l.value0.value3.value2,l.value0.value3.value3,l.value0.value4,l.value0.value5,C)))):l.value0 instanceof Gi&&l.value0.value2 instanceof We?(_=!0,cn(t)(l.value1)(new We(new ve(C,l.value0.value0,l.value0.value1,l.value0.value2.value0),l.value0.value2.value1,l.value0.value2.value2,new ve(l.value0.value2.value3,l.value0.value2.value4,l.value0.value2.value5,l.value0.value2.value6),l.value0.value3,l.value0.value4,l.value0.value5))):l.value0 instanceof Oo&&l.value0.value0 instanceof We?(_=!0,cn(t)(l.value1)(new We(new ve(l.value0.value0.value0,l.value0.value0.value1,l.value0.value0.value2,l.value0.value0.value3),l.value0.value0.value4,l.value0.value0.value5,new ve(l.value0.value0.value6,l.value0.value1,l.value0.value2,C),l.value0.value3,l.value0.value4,l.value0.value5))):l.value0 instanceof Oo&&l.value0.value5 instanceof We?(_=!0,cn(t)(l.value1)(new We(l.value0.value0,l.value0.value1,l.value0.value2,new ve(C,l.value0.value3,l.value0.value4,l.value0.value5.value0),l.value0.value5.value1,l.value0.value5.value2,new ve(l.value0.value5.value3,l.value0.value5.value4,l.value0.value5.value5,l.value0.value5.value6)))):l.value0 instanceof Bi&&l.value0.value3 instanceof We?(_=!0,cn(t)(l.value1)(new We(l.value0.value0,l.value0.value1,l.value0.value2,new ve(l.value0.value3.value0,l.value0.value3.value1,l.value0.value3.value2,l.value0.value3.value3),l.value0.value3.value4,l.value0.value3.value5,new ve(l.value0.value3.value6,l.value0.value4,l.value0.value5,C)))):(_=!0,pu("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[l.constructor.name])}for(;!_;)v=D(s,m);return v}},n=function(i){return function(m){var s=i,_=!1,v;function D(l,C){if(C instanceof ve&&C.value0 instanceof Qt&&C.value3 instanceof Qt)return _=!0,r(l)(Qt.value);if(C instanceof ve){s=new se(new fi(C.value0,C.value1,C.value2),l),m=C.value3;return}if(C instanceof We&&C.value0 instanceof Qt&&C.value3 instanceof Qt&&C.value6 instanceof Qt)return _=!0,r(new se(new fi(Qt.value,C.value1,C.value2),l))(Qt.value);if(C instanceof We){s=new se(new Bi(C.value0,C.value1,C.value2,C.value3,C.value4,C.value5),l),m=C.value6;return}return _=!0,pu("The impossible happened in partial function `removeMaxNode`.")}for(;!_;)v=D(s,m);return v}},a=function(i){var m=!1,s;function _(v){if(v instanceof ve&&v.value3 instanceof Qt)return m=!0,{key:v.value1,value:v.value2};if(v instanceof ve){i=v.value3;return}if(v instanceof We&&v.value6 instanceof Qt)return m=!0,{key:v.value4,value:v.value5};if(v instanceof We){i=v.value6;return}return m=!0,pu("The impossible happened in partial function `maxNode`.")}for(;!m;)s=_(i);return s},u=ee(t),f=function(i){return function(m){var s=i,_=!1,v;function D(l,C){if(C instanceof Qt)return _=!0,V.value;if(C instanceof ve){var ot=u(e)(C.value1);if(C.value3 instanceof Qt&&ot instanceof Ae)return _=!0,new B(new nt(C.value2,r(l)(Qt.value)));if(ot instanceof Ae){var vt=a(C.value0);return _=!0,new B(new nt(C.value2,n(new se(new Wi(vt.key,vt.value,C.value3),l))(C.value0)))}if(ot instanceof ne){s=new se(new Wi(C.value1,C.value2,C.value3),l),m=C.value0;return}s=new se(new fi(C.value0,C.value1,C.value2),l),m=C.value3;return}if(C instanceof We){var Gt=function(){return C.value0 instanceof Qt&&C.value3 instanceof Qt&&C.value6 instanceof Qt}(),ot=u(e)(C.value4),re=u(e)(C.value1);if(Gt&&re instanceof Ae)return _=!0,new B(new nt(C.value2,cn(t)(l)(new ve(Qt.value,C.value4,C.value5,Qt.value))));if(Gt&&ot instanceof Ae)return _=!0,new B(new nt(C.value5,cn(t)(l)(new ve(Qt.value,C.value1,C.value2,Qt.value))));if(re instanceof Ae){var vt=a(C.value0);return _=!0,new B(new nt(C.value2,n(new se(new Gi(vt.key,vt.value,C.value3,C.value4,C.value5,C.value6),l))(C.value0)))}if(ot instanceof Ae){var vt=a(C.value3);return _=!0,new B(new nt(C.value5,n(new se(new Oo(C.value0,C.value1,C.value2,vt.key,vt.value,C.value6),l))(C.value3)))}if(re instanceof ne){s=new se(new Gi(C.value1,C.value2,C.value3,C.value4,C.value5,C.value6),l),m=C.value0;return}if(re instanceof be&&ot instanceof ne){s=new se(new Oo(C.value0,C.value1,C.value2,C.value4,C.value5,C.value6),l),m=C.value3;return}s=new se(new Bi(C.value0,C.value1,C.value2,C.value3,C.value4,C.value5),l),m=C.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[C.constructor.name])}for(;!_;)v=D(s,m);return v}};return f(Or.value)}},ha={foldr:function(t){return function(e){return function(r){if(r instanceof Qt)return e;if(r instanceof ve)return rr(ha)(t)(t(r.value2)(rr(ha)(t)(e)(r.value3)))(r.value0);if(r instanceof We)return rr(ha)(t)(t(r.value2)(rr(ha)(t)(t(r.value5)(rr(ha)(t)(e)(r.value6)))(r.value3)))(r.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[r.constructor.name])}}},foldl:function(t){return function(e){return function(r){if(r instanceof Qt)return e;if(r instanceof ve)return ar(ha)(t)(t(ar(ha)(t)(e)(r.value0))(r.value2))(r.value3);if(r instanceof We)return ar(ha)(t)(t(ar(ha)(t)(t(ar(ha)(t)(e)(r.value0))(r.value2))(r.value3))(r.value5))(r.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[r.constructor.name])}}},foldMap:function(t){return function(e){return function(r){if(r instanceof Qt)return Pt(t);if(r instanceof ve)return gt(t.Semigroup0())(qn(ha)(t)(e)(r.value0))(gt(t.Semigroup0())(e(r.value2))(qn(ha)(t)(e)(r.value3)));if(r instanceof We)return gt(t.Semigroup0())(qn(ha)(t)(e)(r.value0))(gt(t.Semigroup0())(e(r.value2))(gt(t.Semigroup0())(qn(ha)(t)(e)(r.value3))(gt(t.Semigroup0())(e(r.value5))(qn(ha)(t)(e)(r.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[r.constructor.name])}}}},_a={foldrWithIndex:function(t){return function(e){return function(r){if(r instanceof Qt)return e;if(r instanceof ve)return wo(_a)(t)(t(r.value1)(r.value2)(wo(_a)(t)(e)(r.value3)))(r.value0);if(r instanceof We)return wo(_a)(t)(t(r.value1)(r.value2)(wo(_a)(t)(t(r.value4)(r.value5)(wo(_a)(t)(e)(r.value6)))(r.value3)))(r.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[r.constructor.name])}}},foldlWithIndex:function(t){return function(e){return function(r){if(r instanceof Qt)return e;if(r instanceof ve)return ao(_a)(t)(t(r.value1)(ao(_a)(t)(e)(r.value0))(r.value2))(r.value3);if(r instanceof We)return ao(_a)(t)(t(r.value4)(ao(_a)(t)(t(r.value1)(ao(_a)(t)(e)(r.value0))(r.value2))(r.value3))(r.value5))(r.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[r.constructor.name])}}},foldMapWithIndex:function(t){return function(e){return function(r){if(r instanceof Qt)return Pt(t);if(r instanceof ve)return gt(t.Semigroup0())(ai(_a)(t)(e)(r.value0))(gt(t.Semigroup0())(e(r.value1)(r.value2))(ai(_a)(t)(e)(r.value3)));if(r instanceof We)return gt(t.Semigroup0())(ai(_a)(t)(e)(r.value0))(gt(t.Semigroup0())(e(r.value1)(r.value2))(gt(t.Semigroup0())(ai(_a)(t)(e)(r.value3))(gt(t.Semigroup0())(e(r.value4)(r.value5))(ai(_a)(t)(e)(r.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[r.constructor.name])}}},Foldable0:function(){return ha}},dy=function(){return wo(_a)(function(t){return function(e){return function(r){return new se(t,r)}}})(Or.value)}();var sp=function(){return Qt.value}();var Wm=function(t){return function(e){return function(r){return Ur(r)(on)(oF(t)(e)(r))}}};var mp=function(t){return function(e){return function(r){return function(n){var a=e(uF(t)(r)(n));if(a instanceof V)return Wm(t)(r)(n);if(a instanceof B)return Sl(t)(r)(a.value0)(n);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[a.constructor.name])}}}};var iF=function(t){return function(e){return function(r){return function(n){var a=function(u){return function(f){return function(i){return mp(t)(function(){var m=Ur(i)(e(i));return function(s){return B.create(m(s))}}())(u)(f)}}};return ao(_a)(a)(n)(r)}}}};var by=function(t){return iF(t)(T)};var Tl=function(t){return t.partitionMap};var Ui=function(t){return t.filterMap};var xl=function(t){return t.filter};var pF=function(t){return t},Fl=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),$l=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),sF=function(t){return t},vp=wu(),b=sF;var L=function(){return Fl.create}();var ct=function(){return $l.create}(),Qe=function(){var t=p(Ia)(p(x)(T(!0)));return function(e){return pF(t(e))}}(),K=function(t){return t.attr};var Io=function(t){return t.reflectType};var Um={map:function(t){return function(e){return p(xe)(t)(e)}}};var dF=function(t){return Io(t)},df=function(){return function(t){return t}};var hy=function(t){return[t]};var Ty=function(){return function(){return function(){return function(){return function(){return function(t){return function(e){return function(r){return r[dF(t)(e)]}}}}}}}};var qm=[];var bf=function(){return function(){return function(t){return function(e){return Ni(t)(e)}}}};var Ta={dimap:function(t){return function(e){return function(r){return function(n){return e(r(t(n)))}}}}},Ro=function(t){return t.dimap},oo=function(t){return function(e){return Ro(t)(e)(Z(et))}};var AF=function(t){return function(e){return function(r){return by(t)(e)(r)}}};var zm=function(t){return dy(t)};var $y=function(t){return vy(t)(void 0)};var Vm=function(t){return{append:AF(t)}};var My=function(t){return Dy(t)},wy=function(t){return function(e){return function(r){return Sl(t)(e)(void 0)(r)}}};var Py={foldMap:function(t){return function(e){var r=qn(qa)(t)(e);return function(n){return r(zm(n))}}},foldl:function(t){return function(e){var r=ar(qa)(t)(e);return function(n){return r(zm(n))}}},foldr:function(t){return function(e){var r=rr(qa)(t)(e);return function(n){return r(zm(n))}}}};var Jm=sp;var Oy=function(t){return{mempty:Jm,Semigroup0:function(){return Vm(t)}}};var Dp=function(t){return function(e){return function(r){return Wm(t)(e)(r)}}};function Iy(t){return function(e){return function(){return setTimeout(e,t)}}}function Ry(t){return function(){clearTimeout(t)}}var dp=Iy;var kF={eq:function(t){return function(e){return t===e}}},bp={compare:function(t){return function(e){return ee(jr)(t)(e)}},Eq0:function(){return kF}};var wl=Ry;var io=function(t){return t.sampleOn};var br=function(t){return t.keepLatest},Pu=function(t){return t.fold};var Pl=function(t){return function(e){return function(r){return function(n){return Ui(t.Filterable1())(on)(Pu(t)(function(a){return function(u){return p(ti)(h(xo))(e(a)(u.value0))}})(r)(new nt(n,V.value)))}}}},Ap=function(t){return function(e){var r=function(n){return function(a){if(a instanceof V)return new B({now:n,last:V.value});if(a instanceof B)return new B({now:n,last:new B(a.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 56, column 3 - line 56, column 50): "+[n.constructor.name,a.constructor.name])}};return Ui(t.Filterable1())(Z(et))(Pu(t)(r)(e)(V.value))}},Ol=function(t){return t.fix};var On=function(t){return function(e){return function(r){return O(t.Plus0().Alt0())(io(t)(e)(r))(io(t)(r)(p(t.Filterable1().Functor1())(zf)(e)))}}},Y=function(t){return t.bang};function Xm(t){return function(e){return t===e}}var Qm=Xm;var SF=function(t){return t};var St=function(t){return function(e){return t(e)}},hF=function(t){return function(e){return function(r){return function(n){return function(a){return q(t.Monad0().Bind1())(Rt(t)(Br(V.value)))(function(u){return q(t.Monad0().Bind1())(r(function(f){return Rt(t)(Kt(Oi)(an(new B(f))(u)))}))(function(f){return q(t.Monad0().Bind1())(n(function(i){return q(t.Monad0().Bind1())(Rt(t)($r(u)))(nr(e)(Je)(function(m){return a(i(m))}))}))(function(i){return h(e)(j(e.Apply0())(f)(i))})})})}}}}},qt=SF,TF=function(t){return function(e){return function(r){return q(t.Monad0().Bind1())(Rt(t)(Br(V.value)))(function(n){return q(t.Monad0().Bind1())(e(function(a){return Gr(Sr)(t.Monad0().Bind1())(q(t.Monad0().Bind1())(Rt(t)($r(n)))(K_(t.Monad0().Applicative0())(Je)))(function(){return q(t.Monad0().Bind1())(St(a)(r))(function(u){return Rt(t)(Kt(Oi)(an(new B(u))(n)))})})}))(function(a){return h(t.Monad0().Applicative0())(Gr(Sr)(t.Monad0().Bind1())(q(t.Monad0().Bind1())(Rt(t)($r(n)))(K_(t.Monad0().Applicative0())(Je)))(function(){return a}))})})}}},k={map:function(t){return function(e){return function(r){return e(function(n){return r(t(n))})}}}};var xF=function(t){return function(e){return function(r){return function(n){return function(a){return q(t.Monad0().Bind1())(Rt(t)(Br(n)))(function(u){return r(function(f){return q(t.Monad0().Bind1())(Rt(t)(Sa(e(f))(u)))(a)})})}}}}},Il=function(t){return function(e){return function(r){return function(n){return r(function(a){var u=e(a);if(u instanceof B)return n(u.value0);if(u instanceof V)return h(t)(void 0);throw new Error("Failed pattern match at FRP.Event (line 126, column 13 - line 128, column 27): "+[u.constructor.name])})}}}},Km=function(t){return function(e){return Il(t)(function(r){var n=e(r);if(n)return new B(r);if(!n)return V.value;throw new Error("Failed pattern match at FRP.Event (line 84, column 13 - line 86, column 25): "+[n.constructor.name])})}},Ou=function(t){return function(e){return qt(function(r){return function(){var a=_r(Pt(Oy(bp)))(),u=St(e)(function(f){return function(){var m=_r(V.value)(),s=dp(t)(function(){r(f)();var v=wr(m)();return Ur(h(c)(void 0))(function(D){return pf(Dp(bp)(D))(a)})(v)()})();return Mn(new B(s))(m)(),pf(gt(Vm(bp))($y(s)))(a)()}})();return function(){var i=wr(a)();return ea(c)(Py)(i)(wl)(),u()}}})}};var Rl=function(t){return function(e){return q(t.Monad0().Bind1())(Rt(t)(Br([])))(function(r){return h(t.Monad0().Applicative0())({event:function(n){return q(e.Monad0().Bind1())(Rt(e)(Sa(function(a){return gt(Ra)(a)([n])})(r)))(function(){return h(e.Monad0().Applicative0())(q(e.Monad0().Bind1())(Rt(e)(Sa(Mm(Qm)(n))(r)))(function(){return h(e.Monad0().Applicative0())(void 0)}))})},push:function(n){return q(e.Monad0().Bind1())(Rt(e)($r(r)))(nr(e.Monad0().Applicative0())(wt)(function(a){return a(n)}))}})})}},FF=function(t){return function(e){return function(r){return function(n){return q(e.Bind1())(Rl(t)(t))(function(a){var u=r(a.event);return q(e.Bind1())(St(u.input)(a.push))(function(f){return q(e.Bind1())(St(u.output)(n))(function(i){return h(e.Applicative0())(j(e.Bind1().Apply0())(f)(i))})})})}}}},qi=function(t){return function(e){return function(r){return qt(function(n){return q(t.Monad0().Bind1())(Rl(t)(t))(function(a){return Gr(Sr)(t.Monad0().Bind1())(n(r(a.event)))(function(){return St(e)(a.push)})})})}}},Ly=function(t){return{compact:Il(t)(Z(et)),separate:function(e){return{left:Il(t)(function(r){if(r instanceof Yt)return new B(r.value0);if(r instanceof Zt)return V.value;throw new Error("Failed pattern match at FRP.Event (line 67, column 13 - line 69, column 33): "+[r.constructor.name])})(e),right:Il(t)(function(r){if(r instanceof Zt)return new B(r.value0);if(r instanceof Yt)return V.value;throw new Error("Failed pattern match at FRP.Event (line 74, column 13 - line 76, column 32): "+[r.constructor.name])})(e)}}}},mu=function(t){return{filter:Km(t),filterMap:Il(t),partition:function(e){return function(r){return{yes:Km(t)(e)(r),no:Km(t)(function(){var n=lu(Ga);return function(a){return n(e(a))}}())(r)}}},partitionMap:function(e){return function(r){return{left:Ui(mu(t))(function(){var n=La(B.create)(T(V.value));return function(a){return n(e(a))}}())(r),right:Ui(mu(t))(function(n){return U_(e(n))})(r)}}},Compactable0:function(){return Ly(t)},Functor1:function(){return k}}},Zr=function(t){return function(e){return qt(function(r){return q(t.Monad0().Bind1())(Rl(t)(t))(function(n){return Gr(Sr)(t.Monad0().Bind1())(r(e(n.push)(n.event)))(function(){return h(t.Monad0().Applicative0())(h(t.Monad0().Applicative0())(void 0))})})})}},Ft=function(t){return function(e){return function(r){return p(t.Apply0().Functor0())(function(n){return h(t)(void 0)})(r(e))}}},N=function(t){return{alt:function(e){return function(r){return function(n){return zt(t.Apply0())(p(t.Apply0().Functor0())(function(a){return function(u){return j(t.Apply0())(a)(u)}})(e(n)))(r(n))}}},Functor0:function(){return k}}},g=function(t){return{empty:function(e){return h(t)(h(t)(void 0))},Alt0:function(){return N(t)}}},S=function(t){return{fold:xF(t),keepLatest:TF(t),sampleOn:hF(t)(t.Monad0().Applicative0()),fix:FF(t)(t.Monad0()),bang:Ft(t.Monad0().Applicative0()),Plus0:function(){return g(t.Monad0().Applicative0())},Filterable1:function(){return mu(t.Monad0().Applicative0())}}};var yp="_____$__$_$$_vbus";function Ym(t){return t[yp]=yp,t}function Zm(t){return()=>{for(let e in t)delete t[e]}}function tv(t){return()=>{let e=(u,f,i,m)=>{let s=Object.keys(m);for(var _=0;_<s.length;_++)if(m[s[_]]instanceof Object&&m[s[_]][yp]===yp){let v={},D={};e(u,v,D,m[s[_]]),f[s[_]]=v,i[s[_]]=D}else{let v=`${Math.random()}`;u[v]={},f[s[_]]=D=>()=>{let l=Object.keys(u[v]);for(var C=0;C<l.length;C++)u[v][l[C]](D)()},i[s[_]]=D=>()=>{let l=`${Math.random()}`;return u[v][l]=D,()=>{delete u[v][l]}}}},r={},n={},a={};return e(r,n,a,t),{p:n,e:a,s:r}}}function Nl(t,e){var r={};for(var n in e)({}).hasOwnProperty.call(e,n)&&(r[n]=e[n]);for(var a in t)({}).hasOwnProperty.call(t,a)&&(r[a]=t[a]);return r}var Gy=function(t){return function(){return function(){return function(e){return function(r){return function(n){return Ju(Er(t)(e))(r)(n)}}}}}};var By=function(){return function(){return function(t){return function(e){return Nl(t,e)}}}},Ll=function(t){return function(){return function(){return function(e){return function(r){return function(n){return Ju(Er(t)(e))(r)(n)}}}}}},ci=function(t){return function(){return function(e){return function(r){return Ja(Er(t)(e))(r)}}}};var Hn={vb:function(t){return function(e){return function(r){return{}}}}},kp=function(t){return t.vb},vu=function(){return function(t){return function(e){return function(r){return function(n){var a=kp(e)(d.value)(d.value)(d.value);return qt(function(u){return q(t.Monad0().Bind1())(tv(a))(function(f){return Gr(Sr)(t.Monad0().Bind1())(u(n(f.p)(f.e)))(function(){return h(t.Monad0().Applicative0())(Zm(f.s))})})})}}}}},Iu=function(t){return function(){return function(){return function(){return function(e){return function(r){return function(){return function(){return function(){return function(){return{vb:function(n){return function(a){return function(u){return Ll(t)()()(d.value)(Ym(kp(e)(d.value)(d.value)(d.value)))(kp(r)(d.value)(d.value)(d.value))}}}}}}}}}}}}}},ur=function(t){return function(){return function(){return function(e){return function(){return function(){return function(){return function(){return{vb:function(r){return function(n){return function(a){return Ll(t)()()(d.value)(void 0)(kp(e)(d.value)(d.value)(d.value))}}}}}}}}}}}};var rv=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Uy=function(){function t(){}return t.value=new t,t}(),nv=function(){function t(){}return t.value=new t,t}(),av=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),H=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),uv=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),U=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var rt=function(t){return new uv(t)};function qy(t){return function(){var e={};for(var r in t)hasOwnProperty.call(t,r)&&(e[r]=t[r]);return e}}var Ru={};function ov(t){return t()}function Hy(t,e){var r={};for(var n in t)hasOwnProperty.call(t,n)&&(r[n]=e(t[n]));return r}function zy(t,e){var r={};for(var n in t)hasOwnProperty.call(t,n)&&(r[n]=e(n)(t[n]));return r}function Vy(t){return function(e){return function(r){return function(n){var a=r;function u(i){return function(m){return e(m)(i)(n[i])}}for(var f in n)hasOwnProperty.call(n,f)&&(a=t(a)(u(f)));return a}}}}function Wl(t){return function(e){var r=[];for(var n in e)hasOwnProperty.call(e,n)&&r.push(t(n)(e[n]));return r}}var IF=Object.keys||Wl(function(t){return function(){return t}});function iv(t){return function(e){return function(r){return function(){return r[t]=e,r}}}}var fv=function(t){return function(e){return function(){return delete e[t],e}}};var cv=Wl(function(t){return function(e){return e}});var UF=qy;var jy=function(t){return function(e){return ov(function(){var n=UF(e)();return t(n)(),n})}};var Xy=function(t){return function(e){return zy(e,t)}};var Nu=function(t){return function(e){return jy(iv(t)(e))}},gp={map:function(t){return function(e){return Hy(e,t)}}},qF={mapWithIndex:Xy,Functor0:function(){return gp}},lv=function(){return ft};var Cp=Vy(zf),Qy=function(t){return function(e){return Cp(function(r){return function(n){return function(a){return gt(t.Semigroup0())(r)(e(n)(a))}}})(Pt(t))}},yf={foldl:function(t){return Cp(function(e){return function(r){return t(e)}})},foldr:function(t){return function(e){return function(r){return rr(wt)(t)(e)(cv(r))}}},foldMap:function(t){return function(e){return Qy(t)(T(e))}}},Ky={foldlWithIndex:function(t){return Cp(Tt(t))},foldrWithIndex:function(t){return function(e){return function(r){return rr(wt)(Kf(t))(e)(Wl(nt.create)(r))}}},foldMapWithIndex:function(t){return Qy(t)},Foldable0:function(){return yf}},HF={traverseWithIndex:function(t){return function(e){return function(r){return Cp(function(n){return function(a){return function(u){return zt(t.Apply0())(p(t.Apply0().Functor0())(Tt(Nu(a)))(n))(e(a)(u))}}})(h(t)(Ru))(r)}}},FunctorWithIndex0:function(){return qF},FoldableWithIndex1:function(){return Ky},Traversable2:function(){return ac}},ac={traverse:function(t){var e=Li(HF)(t);return function(r){return e(T(r))}},sequence:function(t){return ra(ac)(t)(Z(et))},Functor0:function(){return gp},Foldable1:function(){return yf}};var uc=function(t){return jy(fv(t))};var Wu={proof:function(t){return t},Coercible0:function(){}},_v=function(t){return t.proof};var Yy=function(){function t(){}return t.value=new t,t}(),pv=function(){function t(){}return t.value=new t,t}(),zF=function(){function t(){}return t.value=new t,t}();var VF=function(t){return t.makeText},JF=function(t){return function(e){return function(r){return p(k)(function(n){return t.setText(function(a){return{id:e,text:a}}(n))})(r)}}},jF=function(t){return function(e){return function(r){return p(k)(function(n){return function(a){if(a.value instanceof Fl)return t.setProp({id:e,key:a.key,value:a.value.value0});if(a.value instanceof $l)return t.setCb({id:e,key:a.key,value:a.value.value0});throw new Error("Failed pattern match at Deku.Control (line 73, column 26 - line 75, column 45): "+[a.value.constructor.name])}(vp(n))})(r)}}},XF=function(t){return t.makeElement},ln=function(t){return function(e){var r=function(n){return function(a){return qt(function(u){return q(t.Bind1())(a.ids)(function(f){return Gr(Sr)(t.Bind1())(n.raiseId(f))(function(){return p(t.Bind1().Apply0().Functor0())(j(t.Bind1().Apply0())(u(a.deleteFromCache({id:f}))))(St(Ve(wt)(g(t.Applicative0()))([Ft(t.Applicative0())(VF(a)({id:f,parent:n.parent,scope:n.scope})),JF(a)(f)(e)]))(u))})})})}};return new U(r)}},or=function(t){return function(e){return ln(t)(Ft(t.Applicative0())(e))}},Zy=function(t){return function(e){return function(r){var n=function(a){var u=function(f){return function(i){return new nt(i+1|0,new nt(f,i))}};return Pl(S(t))(u)(a)(0)};return new av(br(S(t))(qi(t)(n(r))(function(a){return p(k)(function(u){return O(N(t.Monad0().Applicative0()))(Ft(t.Monad0().Applicative0())(new rv(e(u.value0))))(p(k)(T(nv.value))(xl(mu(t.Monad0().Applicative0()))(function(){var f=ce(Mi)(u.value1+1|0);return function(i){return f(on(i))}}())(a)))})(a)})))}}},QF=function(t){return function(e){return ft}},J=function(t){return function(e){return function(r){return function(n){var a=function(u){return function(f){return qt(function(i){return q(t.Monad0().Bind1())(f.ids)(function(m){return Gr(Sr)(t.Monad0().Bind1())(u.raiseId(m))(function(){return p(t.Monad0().Bind1().Apply0().Functor0())(j(t.Monad0().Bind1().Apply0())(i(f.deleteFromCache({id:m}))))(St(O(N(t.Monad0().Applicative0()))(Ve(wt)(g(t.Monad0().Applicative0()))([Ft(t.Monad0().Applicative0())(XF(f)({id:m,parent:u.parent,scope:u.scope,tag:e})),jF(f)(m)(r)]))(kf(t.Monad0().Applicative0())(t)({parent:m,scope:u.scope,raiseId:function(s){return h(t.Monad0().Applicative0())(void 0)}})(f)(n)))(i))})})})}};return a}}}},kf=function(t){return function(e){return function(r){return function(n){var a=function(u){return u(r)(n)};return function(u){if(u instanceof H)return Nr(wt)(g(t))(kf(t)(e)(r)(n))(u.value0);if(u instanceof uv)return br(S(e))(p(k)(kf(t)(e)(r)(n))(u.value0));if(u instanceof U)return a(u.value0);if(u instanceof av)return qt(function(f){return q(e.Monad0().Bind1())(Rt(e)(Br(Ru)))(function(i){return q(e.Monad0().Bind1())(St(u.value0)(function(m){return q(e.Monad0().Bind1())(n.ids)(function(s){return q(e.Monad0().Bind1())(Rt(e)(Br(h(t)(void 0))))(function(_){return q(e.Monad0().Bind1())(n.ids)(function(v){return q(e.Monad0().Bind1())(Rt(e)(Br(h(t)(void 0))))(function(D){return q(e.Monad0().Bind1())(Rt(e)(Br(V.value)))(function(l){return q(e.Monad0().Bind1())(Rt(e)(Br(h(t)(void 0))))(function(C){return q(e.Monad0().Bind1())(n.ids)(function(ot){return q(e.Monad0().Bind1())(Rt(e)(Br(Yy.value)))(function(vt){return q(e.Monad0().Bind1())(St(m)(function(Gt){return q(e.Monad0().Bind1())(Rt(e)($r(vt)))(function(re){return Gt instanceof Uy&&re instanceof pv?q(e.Monad0().Bind1())(Rt(e)($r(l)))(nr(t)(Je)(function(he){return f(n.sendToTop(function(gr){return{id:gr}}(he)))})):Gt instanceof nv&&re instanceof pv?Gr(Sr)(e.Monad0().Bind1())(Kt(t.Apply0().Functor0())(Rt(e)(an(zF.value)(vt))))(function(){var he=j(t.Apply0())(j(t.Apply0())(j(t.Apply0())(j(t.Apply0())(q(e.Monad0().Bind1())(Rt(e)($r(l)))(nr(t)(Je)(function(gr){return f(n.disconnectElement({id:gr,parent:r.parent,scope:ot}))})))(Zn(e.Monad0().Bind1())(Rt(e)($r(_)))))(Zn(e.Monad0().Bind1())(Rt(e)($r(D)))))(Kt(t.Apply0().Functor0())(Rt(e)(Sa(uc(s))(i)))))(Kt(t.Apply0().Functor0())(Rt(e)(Sa(uc(v))(i))));return j(t.Apply0())(Kt(t.Apply0().Functor0())(Rt(e)(an(he)(C))))(he)}):Gt instanceof rv&&re instanceof Yy?Gr(Sr)(e.Monad0().Bind1())(Kt(t.Apply0().Functor0())(Rt(e)(an(pv.value)(vt))))(function(){return q(e.Monad0().Bind1())(St(kf(t)(e)({parent:r.parent,scope:ot,raiseId:function(he){return Kt(t.Apply0().Functor0())(Rt(e)(an(new B(he))(l)))}})(n)(function(){return Gt.value0 instanceof U?Gt.value0:new U(J(e)("div")($(g(t)))(Gt.value0))}()))(f))(function(he){return Gr(Sr)(e.Monad0().Bind1())(Kt(t.Apply0().Functor0())(Rt(e)(Sa(Nu(v)(he))(i))))(function(){return Kt(t.Apply0().Functor0())(Rt(e)(an(he)(D)))})})}):h(t)(void 0)})}))(function(Gt){return Gr(Sr)(e.Monad0().Bind1())(Kt(t.Apply0().Functor0())(Rt(e)(an(Gt)(_))))(function(){return Gr(Sr)(e.Monad0().Bind1())(Kt(t.Apply0().Functor0())(Rt(e)(Sa(Nu(s)(Gt))(i))))(function(){return Zn(e.Monad0().Bind1())(Rt(e)($r(C)))})})})})})})})})})})})}))(function(m){return h(t)(Gr(Sr)(e.Monad0().Bind1())(q(e.Monad0().Bind1())(Rt(e)($r(i)))(ar(yf)(j(t.Apply0()))(h(t)(void 0))))(function(){return m}))})})});throw new Error("Failed pattern match at Deku.Control (line 304, column 61 - line 381, column 20): "+[u.constructor.name])}}}}},KF=function(t){return function(e){return function(r){return function(n){return qt(function(a){return q(t.Monad0().Bind1())(n.ids)(function(u){return St(O(N(t.Monad0().Applicative0()))(Ft(t.Monad0().Applicative0())(n.makeRoot({id:u,root:e})))(kf(t.Monad0().Applicative0())(t)({parent:u,scope:"rootScope",raiseId:function(f){return h(t.Monad0().Applicative0())(void 0)}})(n)(QF(Wu)(Wu)(r))))(a)})})}}}};var tk=function(t){return function(e){return function(r){return KF(t)(e)(new H(r))}}};var pr=function(){function t(){}return t.value=new t,t}();var sr={attr:function(t){return function(e){return b({key:"click",value:ct(e)})}}};var Jt=function(){function t(){}return t.value=new t,t}();var Ep={attr:function(t){return function(e){return b({key:"style",value:L(e)})}}};var ek={attr:function(t){return function(e){return b({key:"style",value:L(e)})}}};var pt={attr:function(t){return function(e){return b({key:"style",value:L(e)})}}};var rk={attr:function(t){return function(e){return b({key:"style",value:L(e)})}}},oc={attr:function(t){return function(e){return b({key:"style",value:L(e)})}}};var sv={attr:function(t){return function(e){return b({key:"style",value:L(e)})}}};var nk={attr:function(t){return function(e){return b({key:"style",value:L(e)})}}};var mv=function(t){return function(e){return function(r){return new U(J(t)("a")(e)(new H(r)))}}};var Te=function(t){return function(e){return function(r){return new U(J(t)("div")(e)(new H(r)))}}},Be=function(t){return Te(t)($(g(t.Monad0().Applicative0())))};var fc=function(t){return function(e){return function(r){return new U(J(t)("span")(e)(new H(r)))}}},vv=function(t){return fc(t)($(g(t.Monad0().Applicative0())))};var Dv=(t,e,r)=>{e!=="@portal@"&&r.units[e].main.appendChild(r.units[t].main)},uk=t=>e=>r=>()=>{var n,a=e.id;r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(a),t&&e.parent!=="@portal@"&&(n=document.body.querySelectorAll("[data-deku-ssr-"+a+"]").item(0))?r.units[a]={listeners:{},parent:e.parent,scope:e.scope,main:n}:(r.units[a]={listeners:{},parent:e.parent,scope:e.scope,main:document.createElement(e.tag)},Dv(a,e.parent,r))},ok=t=>e=>r=>()=>{var n=e.id,a;r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(n),t&&e.parent!=="@portal@"&&(a=document.body.querySelectorAll("[data-deku-ssr-"+e.parent+"]").item(0))?r.units[n]={main:a.childNodes[0],parent:e.parent,scope:e.scope}:(r.units[n]={main:document.createTextNode(""),parent:e.parent,scope:e.scope},Dv(n,e.parent,r))};function dv(){return{units:{},scopes:{}}}var ik=t=>e=>r=>()=>{var n=e.id,a=e.value;t&&!r.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(r.units[n]={listeners:{},parent:e.parent,scope:e.scope,main:dom},r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(n)),r.units[n].main.tagName==="INPUT"&&e.key==="value"?r.units[n].main.value=a:r.units[n].main.tagName==="INPUT"&&e.key==="checked"?r.units[n].main.checked=a==="true":r.units[n].main.setAttribute(e.key,a)},fk=t=>e=>r=>()=>{var n=e.id,a=e.value;if(t&&!r.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(r.units[n]={listeners:{},parent:e.parent,scope:e.scope,main:dom},r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(n)),e.key==="@self@")a(r.units[n].main)();else{r.units[n].listeners[e.key]&&r.units[n].main.removeEventListener(e.key,r.units[n].listeners[e.key]);var u=f=>a(f)();r.units[n].main.addEventListener(e.key,u),r.units[n].listeners[e.key]=u}},ck=t=>e=>()=>{var r=t.id;e.units[r].main.nodeValue=t.text},lk=t=>e=>r=>()=>{var n,a,u=e.id,f=e.html,i=e.verb,m=e.cache,s=e.parent,_=e.scope,v=e.pxScope;if(t&&s!=="@portal@"&&(n=document.body.querySelectorAll("[data-deku-ssr-"+u+"]").item(0)))r.units[u]={listeners:{},scope:_,parent:s,main:n};else{let l=Object.entries(m);for(var D=0;D<l.length;D++){let C=l[D][0];l[D][1]===!0?f=f.replace(i+C+i,'data-deku-attr-internal="'+C+'"'):f=f.replace(i+C+i,'<span style="display:contents;" data-deku-elt-internal="'+C+'"></span>')}a=document.createElement("div"),a.innerHTML=f.trim(),r.units[u]={listeners:{},scope:_,parent:s,main:a.firstChild}}r.scopes[_]||(r.scopes[_]=[]),r.scopes[_].push(u),a||(a=n),a.querySelectorAll("[data-deku-attr-internal]").forEach(function(l){var C=l.getAttribute("data-deku-attr-internal");let ot=C+v;r.units[ot]={listeners:{},main:l,scope:_},r.scopes[_].push(ot)}),a.querySelectorAll("[data-deku-elt-internal]").forEach(function(l){var C=l.getAttribute("data-deku-elt-internal");let ot=C+v;r.units[C+v]={listeners:{},main:l,scope:_},r.scopes[_].push(ot)}),n||Dv(u,s,r)},_k=t=>e=>()=>{var r=t.id;e.units[r]={main:t.root}},pk=t=>e=>()=>{var r=t.id,n=t.parent;e.units[r].containingScope=t.scope,e.units[n].main.prepend(e.units[r].main)},sk=t=>e=>()=>{var r=t.id;e.units[r].noop||e.units[r].containingScope&&e.units[r].containingScope!==t.scope||e.units[r].main.remove()},mk=t=>e=>()=>{delete e.units[t.id]},vk=t=>e=>()=>{var r=t.id;e.units[r].main.parentNode.prepend(e.units[r].main)};var Dk=function(t){return function(e){return function(r){return(r|0)===r?t(r):e}}},qe=function(t){return t};var bv=function(t){return function(e){return Math.pow(t,e)|0}};var Sp=isFinite;var Bl=Math.floor;var Vi=function(t){return function(e){return Math.pow(t,e)}},Ul=function(t){return function(e){return t%e}},hp=Math.round;var Tp=Math.sin;var Ji=3.141592653589793;var cc=function(){return Dk(B.create)(V.value)}(),bk=function(t){if(!Sp(t))return 0;if(t>=qe(Gn(cf)))return Gn(cf);if(t<=qe(Bn(cf)))return Bn(cf);if(Ze)return Ca(0)(cc(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},Ak=function(t){return bk(hp(t))};var ql=function(t){return bk(Bl(t))};var Gu=Math.random;var Hl=function(t){return function(e){return function(){var n=Gu(),a=(qe(e)-qe(t)+1)*n+qe(t);return ql(a)}}};var yk=function(t){return t};var u$=1,xp=2147483647,o$=function(){return xp-1|0}(),_c=function(t){var e=function(r){return function(n){return function(a){var u=n-r|0,f=iu($o)(a)(u),i=f<r;return i?f+n|0:f}}};return e(u$)(o$)(t)};var i$=0,f$=48271,kk=function(t){return function(e){return ta()(cc(Ul(qe(f$)*qe(e)+qe(t))(qe(xp))))}},gk=kk(i$);var D$=function(){function t(f){this.fn=f}var e={},r=function(f,i){this.head=f,this.tail=i};function n(f){return new r(f,e)}function a(f){return function(i){return new r(f,i)}}function u(f){for(var i=[],m=f;m!==e;)i.push(m.head),m=m.tail;return i}return function(f){return function(i){return function(m){var s=function(v,D){return f(i(a)(m(v)))(D)},_=function(v,D,l){if(D===0)return v;var C=l[D-1];return new t(function(){var ot=_(s(C,v),D-1,l);return ot})};return function(v){for(var D=i(n)(m(v[v.length-1])),l=_(D,v.length-1,v);l instanceof t;)l=l.fn();return i(u)(l)}}}}}();var Tk=function(t){return t};var $k=Tk,zl=function(t){return t};var Mk=function(t){return $k(ZA(t))};var pc=function(t){if(Mu(t)>0)return new B($k(t));if(Ze)return V.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 157, column 1 - line 157, column 58): "+[t.constructor.name])};var wk=function(t){return function(e){return t(zl(e))}};var Pk=wk(Mu);var Ok=function(){return wk(Fm())};var li=function(t){return t.state};function Lo(t){return new Error(t)}function Ef(t){return function(){throw t}}function Mp(t){return function(e){return function(){try{return e()}catch(r){return r instanceof Error||Object.prototype.toString.call(r)==="[object Error]"?t(r)():t(new Error(r.toString()))()}}}}var co=function(t){return t.throwError};var K$={throwError:Ef,Monad0:function(){return ue}};var Mv={catchError:Tt(Mp),MonadThrow0:function(){return K$}};var _i=function(t){return t.catchError};var Jl=function(t){return function(e){return _i(t)(p(t.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(Zt.create)(e))(function(){var r=h(t.MonadThrow0().Monad0().Applicative0());return function(n){return r(Yt.create(n))}}())}};var ir={liftEffect:Z(et),Monad0:function(){return ue}},mr=function(t){return t.liftEffect};var Pv=function(t){return{map:function(e){return function(r){return function(n){return p(t)(function(a){return new nt(e(a.value0),a.value1)})(r(n))}}}}};var Ov=function(t){return{Applicative0:function(){return Rv(t)},Bind1:function(){return Iv(t)}}},Iv=function(t){return{bind:function(e){return function(r){return function(n){return q(t.Bind1())(e(n))(function(a){var u=r(a.value0);return u(a.value1)})}}},Apply0:function(){return Rp(t)}}},Rp=function(t){return{apply:ju(Ov(t)),Functor0:function(){return Pv(t.Bind1().Apply0().Functor0())}}},Rv=function(t){return{pure:function(e){return function(r){return h(t.Applicative0())(new nt(e,r))}},Apply0:function(){return Rp(t)}}};var Uk=function(t){return{state:function(e){var r=h(t.Applicative0());return function(n){return r(e(n))}},Monad0:function(){return Ov(t)}}};var Hk=function(t){return function(e){var r=t(e);return r.value0}};var uM=function(t){return t};var Vk=function(){var t=function(e){return new nt(yk(e.newSeed),function(){var r={};for(var n in e)({}).hasOwnProperty.call(e,n)&&(r[n]=e[n]);return r.newSeed=gk(e.newSeed),r}())};return li(Uk(Qu))(t)}();var lo=Pv(Fo),Jk=p(lo)(function(t){return qe(t)/qe(xp)})(Vk);var vc=function(t){return Hk(uM(t))};var hf=Iv(Qu);var Tf=Rp(Qu),zk=function(t){return function(e){var r=qe(e),n=qe(t),a=function(i){return n+Ul(i)(r-n+1)},u=p(lo)(qe)(Vk),f=zt(Tf)(p(lo)(Le(ka))(u))(p(lo)(Wn(ka)(2))(u));return p(lo)(function(i){return ql(a(i))})(f)}},Nv=function(t){return function(e){var r=t<=e;return r?zk(t)(e):zk(e)(t)}};var Yl=Rv(Qu);var Lv=function(t){return q(hf)(Nv(0)(Pk(t)-1|0))(function(e){return h(Yl)(Ok()(t)(e))})};var t_=function(t){return t.arbitrary};var jk={arbitrary:Jk};var Xk=function(){return{arbitrary:Nv(-1e6)(1e6)}}();var Qk=function(t){return{ids:function(){var r=wr(t)(),n=Xt(ja)(vc(t_(Xk))({newSeed:_c(r),size:5}));return Kt(x)(jf(Le(xu)(1))(t))(),n},makeElement:uk(!1),makeRoot:_k,makeText:ok(!1),makePursx:lk(!1),setProp:ik(!1),setCb:fk(!1),setText:ck,sendToTop:vk,deleteFromCache:mk,giveNewParent:pk,disconnectElement:sk}};var _M=function(t){return t};var Q=function(t){return{pursxToElement:function(e){return function(r){return function(n){return{cache:Ru,element:function(a){return function(u){return $(g(t))}}}}}}}},Gv=function(t){return t.pursxToElement},_n=function(){return function(t){return function(e){return function(r){return function(n){return{pursxToElement:function(a){return function(u){return function(f){var i=Gv(t)(a)(d.value)(f);return{cache:Nu(Io(e)(d.value))(!0)(i.cache),element:function(m){return function(s){return O(N(n.Monad0().Applicative0()))(p(k)(oo(Ta)(vp)(function(_){if(_.value instanceof Fl)return s.setProp({id:Io(e)(d.value)+a,key:_.key,value:_.value.value0});if(_.value instanceof $l)return s.setCb({id:Io(e)(d.value)+a,key:_.key,value:_.value.value0});throw new Error("Failed pattern match at Deku.Pursx (line 4472, column 38 - line 4482, column 24): "+[_.value.constructor.name])}))(ci(r)()(d.value)(f)))(i.element(m)(s))}}}}}}}}}}}},I=function(){return function(t){return function(e){return function(r){return function(n){return{pursxToElement:function(a){return function(u){return function(f){var i=ci(r)()(d.value)(f),m=Gv(t)(a)(d.value)(f);return{cache:Nu(Io(e)(d.value))(!1)(m.cache),element:function(s){return function(_){return O(N(n.Monad0().Applicative0()))(kf(n.Monad0().Applicative0())(n)({parent:Io(e)(d.value)+a,scope:s.scope,raiseId:function(v){return h(n.Monad0().Applicative0())(void 0)}})(_)(i))(m.element(s)(_))}}}}}}}}}}}};var R=_M,me=function(t){return function(e){return function(){return function(){return function(r){return function(n){return function(a){return function(u){return function(f){var i=function(m){return function(s){return qt(function(_){return q(r.Monad0().Bind1())(s.ids)(function(v){return q(r.Monad0().Bind1())(s.ids)(function(D){return Gr(Sr)(r.Monad0().Bind1())(m.raiseId(v))(function(){var l=Gv(n)(D)(d.value)(f);return p(r.Monad0().Bind1().Apply0().Functor0())(j(r.Monad0().Bind1().Apply0())(_(s.deleteFromCache({id:v}))))(St(O(N(r.Monad0().Applicative0()))(Ft(r.Monad0().Applicative0())(s.makePursx({id:v,parent:m.parent,cache:l.cache,pxScope:D,scope:m.scope,html:Io(t)(u),verb:Io(e)(a)})))(l.element(m)(s)))(_))})})})})}};return new U(i)}}}}}}}}},$t=function(t){return function(){return function(){return function(e){return function(r){return me(t)({reflectType:function(){return"~"}})()()(r)(e)(d.value)}}}}};function Kk(t){var e={};for(var r in t)({}).hasOwnProperty.call(t,r)&&(e[r]=t[r]);return e}function Yk(t){return function(e){return function(r){return r[t]=e,r}}}var Bv=Yo;var Uv=function(){return function(){return function(t){return function(e){return function(r){return function(n){return Yk(Er(t)(e))(r)(n)}}}}}};var qv=et,Zk=function(t){return function(e){return t(Kk(e))}},tg=Tt(Zk)({});var dt=function(){return function(){return{defaults:Tt(By()())}}},sM=function(t){return t.defaults},bt={convertRecordOptions:function(t){return function(e){return function(r){return Z(qv)}}}},rg=function(t){return t.convertRecordOptions},pa=function(t){return t.convertOptionsWithDefaults},At=function(){return function(t){return{convertOptions:function(e){return function(r){return tg(rg(t)(e)(d.value)(r))}}}}},mM=function(t){return t.convertOptions},yt=function(t){return function(e){return{convertOptionsWithDefaults:function(r){return function(n){var a=sM(e)(n),u=mM(t)(r);return function(f){return a(u(f))}}}}}},vM=function(t){return t.convertOption},z=function(t){return function(e){return function(){return function(){return function(){return function(r){return{convertRecordOptions:function(n){return function(a){return function(u){return ho(Bv)(Uv()()(r)(d.value)(vM(e)(n)(d.value)(ci(r)()(d.value)(u))))(rg(t)(n)(d.value)(u))}}}}}}}}}};var dM=function(){return function(){return function(){return function(t){return function(e){return function(r){return W_(r.type)(t)?Ja(r.type)(t)(r.value):e(r)}}}}}};var Ke=function(){return function(t){return function(e){return function(r){return{type:Er(t)(e),value:r}}}}};var bM=function(t){return pu("Data.Variant: pattern match failure ["+(t.type+"]"))},Tr=function(){return function(){return function(){return function(t){return dM()()()(t)(bM)}}}};var $M=function(t){return t};var e_=function(){return Ke()({reflectSymbol:function(){return"nothing"}})(d.value)(void 0)}();var pn=function(){var t=Ke()({reflectSymbol:function(){return"just"}})(d.value);return function(e){return $M(t(e))}}();var r_={foldl:function(t){return function(e){return function(r){return Tr()()()({just:function(n){return t(e)(n)},nothing:function(n){return e}})(r)}}},foldr:function(t){return function(e){return function(r){return Tr()()()({just:function(n){return t(n)(e)},nothing:function(n){return e}})(r)}}},foldMap:function(t){return Y_(r_)(t)}};var zv=function(){var t=ip(Om);return function(e){return fp(t(e))}}();var hz=typeof Array.from=="function",Tz=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",xz=typeof String.prototype.fromCodePoint=="function",Fz=typeof String.prototype.codePointAt=="function";var bu=void 0;var qp=function(t){return t.toInt},pg=function(t){return function(e){return qp(t)(bu)}};var Xa={toInt:function(t){return 8}},sg={Nat0:function(){return Xa}},Go={toInt:function(t){return 7}},mg={Nat0:function(){return Go}},Bo={toInt:function(t){return 6}},vg={Nat0:function(){return Bo}},Fa={toInt:function(t){return 5}},Hp={Nat0:function(){return Fa}},zn={toInt:function(t){return 4}},ua={Nat0:function(){return zn}},Vn={toInt:function(t){return 3}},Au={Nat0:function(){return Vn}},Jn={toInt:function(t){return 2}},yu={Nat0:function(){return Jn}},jn={toInt:function(t){return 1}},ku={Nat0:function(){return jn}},xr={toInt:function(t){return 0}};var Me=function(t){return function(){return function(e){return function(){return function(r){return{Nat0:e.Nat1,Pos1:function(){return t}}}}}}};var _o={Nat0:function(){return Go},Nat1:function(){return Xa}};var po={Nat0:function(){return Bo},Nat1:function(){return Xa}};var so={Nat0:function(){return Fa},Nat1:function(){return Xa}};var mo={Nat0:function(){return zn},Nat1:function(){return Xa}};var sa={Nat0:function(){return zn},Nat1:function(){return Fa}};var vo={Nat0:function(){return Vn},Nat1:function(){return Xa}};var ma={Nat0:function(){return Vn},Nat1:function(){return Fa}};var Do={Nat0:function(){return Jn},Nat1:function(){return Xa}};var va={Nat0:function(){return Jn},Nat1:function(){return Fa}};var bo={Nat0:function(){return jn},Nat1:function(){return Xa}};var Da={Nat0:function(){return jn},Nat1:function(){return Fa}};var Ao={Nat0:function(){return xr},Nat1:function(){return Xa}};var da={Nat0:function(){return xr},Nat1:function(){return Fa}};var Dg={Nat0:function(){return xr},Nat1:function(){return Xa}};var Jv={Nat0:function(){return xr},Nat1:function(){return Go}};var jv={Nat0:function(){return xr},Nat1:function(){return Bo}};var a_={Nat0:function(){return xr},Nat1:function(){return Fa}};var Ha={Nat0:function(){return xr},Nat1:function(){return zn}};var An={Nat0:function(){return xr},Nat1:function(){return Vn}};var yn={Nat0:function(){return xr},Nat1:function(){return Jn}};var kn={Nat0:function(){return xr},Nat1:function(){return jn}},gu={Nat0:function(){return xr},Nat1:function(){return xr}};var dg=ei;var zp=function(t){return t};var u_=function(t){return function(){return function(e){return function(r){return e[qp(t)(r)]}}}};var Vp=function(t){return function(e){var r=pg(t)(d.value),n=function(){return r===0?[]:dn(0)(r-1|0)}();return p(xe)(e)(n)}};var Uu=[];var we=function(t){return function(e){return function(r){return Ni(e)(r)}}};var gn={first:function(t){return function(e){return new nt(t(e.value0),e.value1)}},second:p(ti),Profunctor0:function(){return Ta}},Xn=function(t){return t.second},Jp=function(t){return t.first};var iw=function(t){return function(e){return function(r){return function(n){return Ro(r)(t)(e)(n)}}}};var kg=function(){return function(){return function(t){return iw(wu())(wu())(t)}}};var gg=function(){return function(){return function(t){return kg()()(t)}}};var lw=function(t){return function(e){return function(r){return Ro(e.Profunctor0())(t)(function(n){return n.value1(n.value0)})(Jp(e)(r))}}},Cg=function(t){return function(e){return function(r){return lw(function(n){return new nt(t(n),function(a){return e(n)(a)})})(r)}}};var Eg=function(t){return function(){return function(){return function(e){return function(r){return Cg(ci(t)()(e))(Tt(Gy(t)()()(e)))(r)}}}}};var Sg=function(t){return t};var vw=JSON.parse;var Dw=JSON.stringify;var jp=function(t){return t};var Xp=function(t){return t};var Qp=function(t){return function(e){return t(e)}},o_=function(t){return{map:function(e){return Qp(p(t)(p(_f)(e)))}}};var Kv=function(t){return{Applicative0:function(){return i_(t)},Bind1:function(){return Yv(t)}}},Yv=function(t){return{bind:function(e){return function(r){return q(t.Bind1())(e)(La(function(){var n=h(t.Applicative0());return function(a){return n(Yt.create(a))}}())(function(n){var a=r(n);return a}))}},Apply0:function(){return Tg(t)}}},Tg=function(t){return{apply:ju(Kv(t)),Functor0:function(){return o_(t.Bind1().Apply0().Functor0())}}},i_=function(t){return{pure:function(){var e=h(t.Applicative0());return function(r){return jp(e(Zt.create(r)))}}(),Apply0:function(){return Tg(t)}}};var xg=function(t){return{throwError:function(){var e=h(t.Applicative0());return function(r){return jp(e(Yt.create(r)))}}(),Monad0:function(){return Kv(t)}}};var Zv=function(t){return function(e){return{alt:function(r){return function(n){return q(e.Bind1())(r)(function(a){if(a instanceof Zt)return h(e.Applicative0())(new Zt(a.value0));if(a instanceof Yt)return q(e.Bind1())(n)(function(u){if(u instanceof Zt)return h(e.Applicative0())(new Zt(u.value0));if(u instanceof Yt)return h(e.Applicative0())(new Yt(gt(t)(a.value0)(u.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[u.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[a.constructor.name])})}},Functor0:function(){return o_(e.Bind1().Apply0().Functor0())}}}};var tD=function(){var t=wn();return function(e){return t(Xp(e))}}();function $g(t,e,r){return t==null?e:r(t)}var tn=function(t){return $g(t,V.value,B.create)};function f_(t){return Object.prototype.toString.call(t).slice(8,-1)}var Ng=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var uD=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}();var Lg=ft;var oD=function(t){var e=co(xg(t));return function(r){return e(zv(r))}};var iD=function(t){return function(e){return function(r){if(f_(r)===e)return h(i_(t))(Lg(r));if(Ze)return oD(t)(new uD(e,f_(r)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[e.constructor.name,r.constructor.name])}}};var fD=function(t){return iD(t)("String")};var Zp=function(){function t(){}return t.value=new t,t}(),ts=function(){function t(){}return t.value=new t,t}(),Bg=function(){function t(){}return t.value=new t,t}(),Ug=function(){function t(){}return t.value=new t,t}(),lD=function(){function t(){}return t.value=new t,t}(),qg=function(){function t(){}return t.value=new t,t}(),Hg=function(){function t(){}return t.value=new t,t}();var zg=function(t){return t},Vg=function(t){return t};var Jg=function(t){return t};var jg=function(t){return t};var Xg=function(t){return t};var Qg=function(t){return t},Kg=function(t){return t},Yg=function(t){return t},Zg=function(t){return t},tC=function(t){return t};var _D=function(){function t(){}return t.value=new t,t}(),eC=function(){function t(){}return t.value=new t,t}(),rC=function(){function t(){}return t.value=new t,t}(),pD=function(){function t(){}return t.value=new t,t}(),nC=function(){function t(){}return t.value=new t,t}();var es=function(t){return t};var Ac=function(t){return t};var sD=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),En=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),rs=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Ye=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Qw=function(t){return t},ns=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),as=function(){function t(){}return t.value=new t,t}(),aC=function(t){return t};var Kw=function(t){return t},c_=function(t){return t};var Mf={toAudioOnOff:Z(et)};var wf=function(t){return t.toAudioParameter},uC=function(t){return t.toAudioOnOff};var oC=function(t){return sD.create(Qw(t))};var us=function(){return Sg(function(){var t=gg()()(Ta),e=Eg({reflectSymbol:function(){return"o"}})()()(d.value)(gn);return function(r){return t(e(r))}}())},iC=ft;var Yw=function(){var t=Ke()({reflectSymbol:function(){return"unit"}})(d.value);return function(e){return c_(t(e))}}();var Zw=function(t){return function(e){return{toAudioParameter:function(r){return Yw(r)}}}},fC=function(t){return function(e){return{toAudioParameter:function(){var r=wf(Zw(t)(e));return function(n){return r(Kw(function(a){return{u:a}}(n)))}}()}}},cC=function(){return Ke()({reflectSymbol:function(){return"2x"}})(d.value)(void 0)}(),lC=function(){var t=Ke()({reflectSymbol:function(){return"sudden"}})(d.value);return function(e){return c_(t(e))}}();var _C={toAudioParameter:lC},os={toAudioParameter:function(t){return lC({n:t})}},mD=function(){return Ke()({reflectSymbol:function(){return"step"}})(d.value)(void 0)}();var vD=function(){return Ke()({reflectSymbol:function(){return"on"}})(d.value)(void 0)}(),l_={x:vD,o:0},_t=function(){return Y(S(o))(fn()(Ke()({reflectSymbol:function(){return"onOff"}})(d.value)(l_)))};var pC=function(){return Ke()({reflectSymbol:function(){return"off"}})(d.value)(void 0)}();var tP=function(){var t=Ke()({reflectSymbol:function(){return"numeric"}})(d.value);return function(e){return c_(t(e))}}();var Lr={toAudioParameter:tP};var Uo=function(){return Ke()({reflectSymbol:function(){return"linear"}})(d.value)(void 0)}();var sC=function(){return Ke()({reflectSymbol:function(){return"exponential"}})(d.value)(void 0)}(),eP=function(){var t=Ke()({reflectSymbol:function(){return"envelope"}})(d.value);return function(e){return c_(t(e))}}();var Rn={toAudioParameter:eP},rP=function(){var t=Ke()({reflectSymbol:function(){return"cancel"}})(d.value);return function(e){return c_(t(e))}}();var mC={toAudioParameter:rP};var nP=function(){function t(){}return t.value=new t,t}(),aP=function(){function t(){}return t.value=new t,t}(),uP=function(){function t(){}return t.value=new t,t}(),oP=function(){function t(){}return t.value=new t,t}(),iP=function(){function t(){}return t.value=new t,t}(),fP=function(){function t(){}return t.value=new t,t}(),cP=function(){function t(){}return t.value=new t,t}(),lP=function(){function t(){}return t.value=new t,t}(),_P=function(){function t(){}return t.value=new t,t}(),pP=function(){function t(){}return t.value=new t,t}(),sP=function(){function t(){}return t.value=new t,t}(),mP=function(){function t(){}return t.value=new t,t}(),vP=function(){function t(){}return t.value=new t,t}(),DP=function(){function t(){}return t.value=new t,t}(),pi=function(t){return{toPeriodicOscSpec:function(e){return Ke()({reflectSymbol:function(){return"realImg"}})(d.value)({real:zp(e.value0),img:zp(e.value1)})}}};var is={toInitializeTriangleOsc:function(t){return tC(function(e){return{frequency:e}}(t))}};var vC={toInitializeStereoPanner:function(t){return Zg(function(e){return{pan:e}}(t))}};var yc={toInitializeSquareOsc:function(t){return Yg(function(e){return{frequency:e}}(t))}};var Xi={toInitializeSinOsc:function(t){return Kg(function(e){return{frequency:e}}(t))}};var DC={toInitializeSawtoothOsc:function(t){return Qg(function(e){return{frequency:e}}(t))}};var DD={toInitializeRecorder:function(t){return zg(function(e){return{cb:e}}(t))}};var __={toInitializeMicrophone:function(t){return Vg(function(e){return{microphone:e}}(t))}};var dC=function(t){return function(e){return{toInitializeIIRFilter:function(r){return function(n){return function(a){return{feedforward:_v(Wu)(wu()(r.value0)),feedback:_v(Wu)(wu()(r.value1))}}}}}}};var it={toInitializeGain:function(t){return Xg(function(e){return{gain:e}}(t))}};var bC={toInitializeConvolver:function(t){return Jg(function(e){return{buffer:e}}(t))}},fs={toInitializeConstant:function(t){return jg(function(e){return{offset:e}}(t))}};var dP={convertOption:function(t){return function(e){return Z(et)}}},p_={convertOption:function(t){return function(e){return Z(et)}}},AC={convertOption:function(t){return function(e){return Z(et)}}},yC={convertOption:function(t){return function(e){return pn}}},kC={convertOption:function(t){return function(e){return Z(et)}}},si={convertOption:function(t){return function(e){return Z(et)}}},kc={convertOption:function(t){return function(e){return Z(et)}}},gc={convertOption:function(t){return function(e){return Z(et)}}},Cc={convertOption:function(t){return function(e){return Z(et)}}},Ec={convertOption:function(t){return function(e){return Z(et)}}},Sc={convertOption:function(t){return function(e){return Z(et)}}},gC={convertOption:function(t){return function(e){return Z(et)}}},CC={convertOption:function(t){return function(e){return Z(et)}}},EC={convertOption:function(t){return function(e){return Z(et)}}},dD={convertOption:function(t){return function(e){return Z(et)}}},Pf={convertOption:function(t){return function(e){return Z(et)}}},s_={convertOption:function(t){return function(e){return Z(et)}}},m_={convertOption:function(t){return function(e){return Z(et)}}};var hc={convertOption:function(t){return function(e){return Z(et)}}},SC={convertOption:function(t){return function(e){return Z(et)}}},hC={convertOption:function(t){return function(e){return Z(et)}}},TC={convertOption:function(t){return function(e){return Z(et)}}},bD={convertOption:function(t){return function(e){return Z(et)}}};var xC={convertOption:function(t){return function(e){return Z(et)}}},AD={convertOption:function(t){return function(e){return Z(et)}}},Sn={convertOption:function(t){return function(e){return Z(et)}}},sn={convertOption:function(t){return function(e){return Z(et)}}},yD={convertOption:function(t){return function(e){return Z(et)}}},cs={convertOption:function(t){return function(e){return Z(et)}}},bP=function(t){return t.toPeriodicOscSpec},mi=function(t){return{convertOption:function(e){return function(r){return bP(t)}}}},kD=function(t){return t.toInitializeWaveShaper},FC=function(t){return t.toInitializeTriangleOsc},$C=function(t){return t.toInitializeStereoPanner},MC=function(t){return t.toInitializeSquareOsc},wC=function(t){return t.toInitializeSinOsc},PC=function(t){return t.toInitializeSawtoothOsc},OC=function(t){return t.toInitializeRecorder},gD=function(t){return t.toInitializePlayBuf},IC=function(t){return t.toInitializePeriodicOsc},RC=function(t){return t.toInitializePeaking},NC=function(t){return t.toInitializeNotch},LC=function(t){return t.toInitializeMicrophone},WC=function(t){return t.toInitializeLowshelf},CD=function(t){return t.toInitializeLowpass},ED=function(t){return t.toInitializeLoopBuf},GC=function(t){return t.toInitializeIIRFilter},BC=function(t){return t.toInitializeHighshelf},SD=function(t){return t.toInitializeHighpass},UC=function(t){return t.toInitializeGain},qC=function(t){return t.toInitializeDynamicsCompressor},hD=function(t){return t.toInitializeDelay},HC=function(t){return t.toInitializeConvolver},zC=function(t){return t.toInitializeConstant},TD=function(t){return t.toInitializeBandpass},xD=function(t){return t.toInitializeAllpass};var AP={oversample:cC},yP=function(t){return{toInitializeWaveShaper:function(e){return pa(t)(nP.value)(AP)(e)}}},VC={toInitializeWaveShaper:function(){var t=kD(yP(yt(At()(z(bt)(dP)()()()({reflectSymbol:function(){return"curve"}})))(dt()())));return function(e){return t(function(r){return{curve:r}}(e))}}()},kP={bufferOffset:0,playbackRate:1,duration:e_},v_=function(t){return{toInitializePlayBuf:function(e){return pa(t)(aP.value)(kP)(e)}}},za={toInitializePlayBuf:function(){var t=gD(v_(yt(At()(z(bt)(p_)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())));return function(e){return t(function(r){return{buffer:r}}(e))}}()},gP={},vi=function(t){return{toInitializePeriodicOsc:function(e){return pa(t)(uP.value)(gP)(e)}}},CP={q:1,gain:0},Tc=function(t){return{toInitializePeaking:function(e){return pa(t)(oP.value)(CP)(e)}}};var EP={q:1},xc=function(t){return{toInitializeNotch:function(e){return pa(t)(iP.value)(EP)(e)}}};var SP={gain:0},JC=function(t){return{toInitializeLowshelf:function(e){return pa(t)(fP.value)(SP)(e)}}};var hP={q:1},FD=function(t){return{toInitializeLowpass:function(e){return pa(t)(cP.value)(hP)(e)}}},ls={toInitializeLowpass:function(){var t=CD(FD(yt(At()(z(bt)(dD)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},TP={loopStart:0,loopEnd:0,playbackRate:1,duration:e_},Of=function(t){return{toInitializeLoopBuf:function(e){return pa(t)(lP.value)(TP)(e)}}},ge={toInitializeLoopBuf:function(){var t=ED(Of(yt(At()(z(bt)(Pf)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())));return function(e){return t(function(r){return{buffer:r}}(e))}}()},xP={gain:0},jC=function(t){return{toInitializeHighshelf:function(e){return pa(t)(_P.value)(xP)(e)}}};var FP={q:1},$D=function(t){return{toInitializeHighpass:function(e){return pa(t)(pP.value)(FP)(e)}}},Qa={toInitializeHighpass:function(){var t=SD($D(yt(At()(z(bt)(bD)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},$P=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),XC=function(t){return{toInitializeDynamicsCompressor:function(e){return pa(t)(sP.value)($P)(e)}}},MP={maxDelayTime:1},MD=function(t){return{toInitializeDelay:function(e){return pa(t)(mP.value)(MP)(e)}}},en={toInitializeDelay:function(){var t=hD(MD(yt(At()(z(bt)(AD)()()()({reflectSymbol:function(){return"delayTime"}})))(dt()())));return function(e){return t(function(r){return{delayTime:r}}(e))}}()},wP={q:1},mn=function(t){return{toInitializeBandpass:function(e){return pa(t)(vP.value)(wP)(e)}}},wD={toInitializeBandpass:function(){var t=TD(mn(yt(At()(z(bt)(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},PP={q:1},_s=function(t){return{toInitializeAllpass:function(e){return pa(t)(DP.value)(PP)(e)}}},PD={toInitializeAllpass:function(){var t=xD(_s(yt(At()(z(bt)(cs)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()};function OD(t){return()=>t.slice()}function ID(t){return e=>r=>()=>{r[t]=e}}function RD(t){return()=>t.slice()}var qo=function(){function t(){this.head=null,this.last=null,this.size=0}function e(_,v){this.queue=_,this.value=v,this.next=null,this.prev=null}function r(_){this.draining=!1,this.error=null,this.value=_,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function a(_){try{_()}catch(v){setTimeout(function(){throw v},0)}}function u(_,v){var D=new e(_,v);switch(_.size){case 0:_.head=D;break;case 1:D.prev=_.head,_.head.next=D,_.last=D;break;default:D.prev=_.last,_.last.next=D,_.last=D}return _.size++,D}function f(_){var v;switch(_.size){case 0:return null;case 1:v=_.head,_.head=null;break;case 2:v=_.last,_.head.next=null,_.last=null;break;default:v=_.last,_.last=v.prev,_.last.next=null}return v.prev=null,v.queue=null,_.size--,v.value}function i(_){var v;switch(_.size){case 0:return null;case 1:v=_.head,_.head=null;break;case 2:v=_.head,_.last.prev=null,_.head=_.last,_.last=null;break;default:v=_.head,_.head=v.next,_.head.prev=null}return v.next=null,v.queue=null,_.size--,v.value}function m(_){if(_.queue!==null){if(_.queue.last===_){f(_.queue);return}if(_.queue.head===_){i(_.queue);return}_.prev&&(_.prev.next=_.next),_.next&&(_.next.prev=_.prev),_.queue.size--,_.queue=null,_.value=null,_.next=null,_.prev=null}}function s(_,v){if(!v.draining){var D=v.puts,l=v.takes,C=v.reads,ot,vt,Gt,re,he;for(v.draining=!0;;){if(ot=null,vt=null,Gt=null,re=v.value,he=C.size,v.error!==null){for(re=_.left(v.error);ot=i(D);)a(ot.cb(re));for(;vt=i(C);)a(vt(re));for(;Gt=i(l);)a(Gt(re));break}if(re===n&&(ot=i(D))&&(v.value=re=ot.value),re!==n){for(Gt=i(l);he--&&(vt=i(C));)a(vt(_.right(re)));Gt!==null&&(v.value=n,a(Gt(_.right(re))))}if(ot!==null&&a(ot.cb(_.right(void 0))),v.value===n&&D.size===0||v.value!==n&&l.size===0)break}v.draining=!1}}return r.EMPTY=n,r.putLast=u,r.takeLast=f,r.takeHead=i,r.deleteCell=m,r.drainVar=s,r}();function D_(){return new qo(qo.EMPTY)}function QC(t,e,r){return function(){var n=qo.putLast(e.takes,r);return qo.drainVar(t,e),function(){qo.deleteCell(n)}}}function KC(t,e,r){return function(){return r.value===qo.EMPTY&&r.error===null?(r.value=e,qo.drainVar(t,r),!0):!1}}function YC(t,e){return function(){var r=e.value;return r===qo.EMPTY?t.nothing:(e.value=qo.EMPTY,qo.drainVar(t,e),t.just(r))}}var LP=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),WP=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),GP=function(){function t(){}return t.value=new t,t}();var ND=function(){return{left:Yt.create,right:Zt.create,nothing:V.value,just:B.create,killed:LP.create,filled:WP.create,empty:GP.value}}();var ZC=function(t){return function(e){return QC(ND,t,e)}},ps=function(t){return function(e){return KC(ND,t,e)}};var tE=function(t){return YC(ND,t)};var BP=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}},rE=function(){function t(){}return t.value=new t,t}(),nE=function(){function t(){}return t.value=new t,t}(),UP=function(){function t(){}return t.value=new t,t}();var qP=function(){function t(){}return t.value=new t,t}();var ss={convertOption:function(t){return function(e){return Z(et)}}},ms={convertOption:function(t){return function(e){return Z(et)}}};var HP=function(t){return t.toInitializeAnalyser};var zP=function(){return{cb:function(t){return h(c)(h(c)(void 0))},fftSize:lD.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:pD.value,channelInterpretation:_D.value}}(),vs=function(t){return{toInitializeAnalyser:function(e){return pa(t)(qP.value)(zP)(e)}}};var VP=function(t){return function(e){var r=LC(t)(e),n=function(a){return function(u){return qt(function(f){return function(){var m=u.ids();return a.raiseId(m)(),p(x)(function(s){return j(tt)(f(u.deleteFromCache({id:m})))(s)})(Tt(St)(f)(Ft(c)(u.makeMicrophone({id:m,parent:a.parent,scope:a.scope,microphone:r.microphone}))))()}})}};return new Ye(n)}},d_=function(t){return VP(t)};var Ot=function(t){return function(e){return function(r){return Hr(t)(e)($(g(c)))(r)}}},Hr=function(t){return function(e){return function(r){return function(n){var a=UC(t)(e),u=function(f){return function(i){return qt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(St)(m)(O(N(c))(Ft(c)(i.makeGain({id:_,parent:f.parent,scope:f.scope,gain:a.gain})))(O(N(c))(br(S(o))(p(k)(function(v){return Tr()()()({gain:aE(594)(f.scope)(i)(function(D){return i.setGain(function(l){return{id:_,gain:l}}(D))})})(v)})(r)))(qr({parent:pn(_),scope:f.scope,raiseId:Pt(Xr(Qr(un)))})(i)(new En(n))))))()}})}};return new Ye(u)}}}},qr=function(t){return function(e){var r=function(n){return n(t)(e)};return function(n){if(n instanceof En)return Nr(wt)(g(c))(qr(t)(e))(n.value0);if(n instanceof rs)return br(S(o))(p(k)(qr(t)(e))(n.value0));if(n instanceof Ye)return r(n.value0);if(n instanceof sD)return qt(function(a){return function(){var f=Rt(o)(Br(Ru))(),i=St(n.value0)(function(m){return function(){var _=e.ids(),v=Rt(o)(Br(h(c)(void 0)))(),D=e.ids(),l=Rt(o)(Br(h(c)(void 0)))(),C=Rt(o)(Br(V.value))(),ot=Rt(o)(Br(h(c)(void 0)))(),vt=e.ids(),Gt=Rt(o)(Br(rE.value))(),re=St(m)(function(he){return function(){var lt=Rt(o)($r(Gt))();if(he instanceof as&&lt instanceof nE){Kt(x)(Rt(o)(an(UP.value)(Gt)))();var oe=j(tt)(j(tt)(j(tt)(j(tt)(q(hr)(Rt(o)($r(C)))(nr(c)(Je)(function(Bt){return ea(c)(r_)(t.parent)(function(vr){return a(e.disconnectXFromY({from:Bt,to:vr}))})})))(Zn(hr)(Rt(o)($r(v)))))(Zn(hr)(Rt(o)($r(l)))))(Kt(x)(Rt(o)(Sa(uc(_))(f)))))(Kt(x)(Rt(o)(Sa(uc(D))(f))));return j(tt)(Kt(x)(Rt(o)(an(oe)(ot))))(oe)()}if(he instanceof ns&&lt instanceof rE){Kt(x)(Rt(o)(an(nE.value)(Gt)))();var ke=St(qr({parent:t.parent,scope:vt,raiseId:function(Bt){return Kt(x)(Rt(o)(an(new B(Bt))(C)))}})(e)(function(){return he.value0 instanceof Ye?he.value0:Ot(it)(1)([he.value0])}()))(a)();return Kt(x)(Rt(o)(Sa(Nu(D)(ke))(f)))(),Kt(x)(Rt(o)(an(ke)(l)))()}return void 0}})();return Kt(x)(Rt(o)(an(re)(v)))(),Kt(x)(Rt(o)(Sa(Nu(_)(re))(f)))(),Zn(hr)(Rt(o)($r(ot)))()}})();return function(){return q(hr)(Rt(o)($r(f)))(ar(yf)(j(tt))(h(c)(void 0)))(),i()}}});throw new Error("Failed pattern match at WAGS.Control (line 1771, column 53 - line 1846, column 20): "+[n.constructor.name])}}},aE=BP("tmpResolveAU","WAGS.Control",function(){var t=function(){var f=Ke()({reflectSymbol:function(){return"unit"}})(d.value);return function(i){return Ac(f(i))}}(),e=function(){var f=Ke()({reflectSymbol:function(){return"sudden"}})(d.value);return function(i){return Ac(f(i))}}(),r=function(){var f=Ke()({reflectSymbol:function(){return"numeric"}})(d.value);return function(i){return Ac(f(i))}}(),n=function(){var f=Ke()({reflectSymbol:function(){return"envelope"}})(d.value);return function(i){return Ac(f(i))}}(),a=function(){var f=Ke()({reflectSymbol:function(){return"cancel"}})(d.value);return function(i){return Ac(f(i))}}(),u=function(f){return function(i){return function(m){return function(s){return Tr()()()({numeric:function(){var _=Ft(c);return function(v){return _(m(r(v)))}}(),envelope:function(){var _=Ft(c);return function(v){return _(m(n(v)))}}(),cancel:function(){var _=Ft(c);return function(v){return _(m(a(v)))}}(),sudden:function(){var _=Ft(c);return function(v){return _(m(e(v)))}}(),unit:function(_){var v=Ot(it)(1)([_.u]);return qt(function(D){return function(){var C=D_();return St(O(N(c))(qr({parent:e_,scope:f,raiseId:function(ot){return Kt(x)(ps(ot)(C))}})(i)(v))(qt(function(ot){return function(){return Kt(x)(ZC(C)(function(Gt){if(Gt instanceof Yt)return Ef(Gt.value0);if(Gt instanceof Zt)return ot(m(t({i:Gt.value0})));throw new Error("Failed pattern match at WAGS.Control (line 1744, column 39 - line 1747, column 66): "+[Gt.constructor.name])}))(),h(c)(void 0)}})))(D)()}})}})(s)}}}};return u}),fr=aE(1723),JP=function(t){return function(e){return function(r){return function(n){var a=HP(t)(e),u=function(f){return function(i){return qt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(St)(m)(O(N(c))(Ft(c)(i.makeAnalyser({id:_,parent:f.parent,scope:f.scope,cb:a.cb,fftSize:bv(2)(function(){if(a.fftSize instanceof Zp)return 7;if(a.fftSize instanceof ts)return 8;if(a.fftSize instanceof Bg)return 9;if(a.fftSize instanceof Ug)return 10;if(a.fftSize instanceof lD)return 11;if(a.fftSize instanceof qg)return 12;if(a.fftSize instanceof Hg)return 13;throw new Error("Failed pattern match at WAGS.Control (line 192, column 21 - line 199, column 34): "+[a.fftSize.constructor.name])}()),maxDecibels:a.maxDecibels,minDecibels:a.minDecibels,smoothingTimeConstant:a.smoothingTimeConstant,channelCount:a.channelCount,channelCountMode:function(){if(a.channelCountMode instanceof nC)return"explicit";if(a.channelCountMode instanceof pD)return"max";if(a.channelCountMode instanceof rC)return"clamped-max";throw new Error("Failed pattern match at WAGS.Control (line 205, column 35 - line 208, column 46): "+[a.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(a.channelInterpretation instanceof _D)return"speakers";if(a.channelInterpretation instanceof eC)return"discrete";throw new Error("Failed pattern match at WAGS.Control (line 209, column 40 - line 211, column 41): "+[a.channelInterpretation.constructor.name])}()})))(O(N(c))(p(k)(function(v){return Tr()()()({cb:function(D){return i.setAnalyserNodeCb({id:_,cb:D})}})(v)})(r))(qr({parent:pn(_),scope:f.scope,raiseId:Pt(Xr(Qr(un)))})(i)(new En(n))))))()}})}};return new Ye(u)}}}},Ds=function(t){return function(e){return JP(t)(e)($(g(c)))}},uE=function(t){return function(e){return function(r){var n=HC(t)(e),a=function(u){return function(f){return qt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(St)(i)(O(N(c))(Ft(c)(f.makeConvolver({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer})))(qr({parent:pn(s),scope:u.scope,raiseId:Pt(Xr(Qr(un)))})(f)(new En(r)))))()}})}};return new Ye(a)}}},Ka=function(t){var e=function(r){return function(n){return qt(function(a){return function(){var f=Rt(o)(Br(V.value))(),i=t(new Ye(function(m){return function(s){return qt(function(_){return function(){return function(){var l=Rt(o)($r(f))();if(l instanceof V)return void 0;if(l instanceof B)return ea(c)(r_)(m.parent)(function(C){return $n(c)(l.value0!==C)(j(tt)(m.raiseId(l.value0))(_(n.connectXToY({from:l.value0,to:C}))))})();throw new Error("Failed pattern match at WAGS.Control (line 1651, column 36 - line 1655, column 82): "+[l.constructor.name])}(),h(c)(void 0)}})}}));return St(qr({parent:r.parent,scope:r.scope,raiseId:function(m){return function(){return r.raiseId(m)(),Kt(x)(Rt(o)(an(new B(m))(f)))()}}})(n)(i))(a)()}})}};return new Ye(e)};var jP=function(){return function(){return function(t){return function(e){return function(r){return function(n){return function(a){var u=GC(t)(n)(e)(r),f=function(i){return function(m){return qt(function(s){return function(){var v=m.ids();return i.raiseId(v)(),p(x)(function(D){return j(tt)(s(m.deleteFromCache({id:v})))(D)})(Tt(St)(s)(O(N(c))(Ft(c)(m.makeIIRFilter({id:v,parent:i.parent,scope:i.scope,feedforward:df()(u.feedforward),feedback:df()(u.feedback)})))(qr({parent:pn(v),scope:i.scope,raiseId:Pt(Xr(Qr(un)))})(m)(new En(a)))))()}})}};return new Ye(f)}}}}}}},oE=function(){return function(){return function(t){return jP()()(t)(d.value)(d.value)}}},LD=function(t){return function(e){return function(r){var n=OC(t)(e),a=function(u){return function(f){return qt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(St)(i)(O(N(c))(Ft(c)(f.makeRecorder({id:s,parent:u.parent,scope:u.scope,cb:n.cb})))(qr({parent:pn(s),scope:u.scope,raiseId:Pt(Xr(Qr(un)))})(f)(r))))()}})}};return new Ye(a)}}},XP=function(t){return function(e){return qt(function(r){return function(){var a=e.ids();return r(e.makeSpeaker({id:a}))(),St(qr({parent:pn(a),scope:"toplevel",raiseId:Pt(Xr(Qr(un)))})(e)(new En(t)))(r)()}})}},If=XP,QP=function(t){return function(e){return function(r){var n=ED(t)(e),a=function(u){return function(f){return qt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(St)(i)(O(N(c))(Ft(c)(f.makeLoopBuf({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer,playbackRate:n.playbackRate,loopStart:n.loopStart,loopEnd:n.loopEnd,duration:n.duration})))(br(S(o))(p(k)(function(_){return Tr()()()({buffer:function(v){return Ft(c)(f.setBuffer({id:s,buffer:v}))},playbackRate:fr(u.scope)(f)(function(v){return f.setPlaybackRate(function(D){return{id:s,playbackRate:D}}(v))}),loopStart:function(v){return Ft(c)(f.setLoopStart({id:s,loopStart:v}))},loopEnd:function(v){return Ft(c)(f.setLoopEnd({id:s,loopEnd:v}))},onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new Ye(a)}}},le=function(t){return QP(t)};var KP=function(t){return function(e){return function(r){var n=IC(t)(e),a=function(u){return function(f){return qt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(St)(i)(O(N(c))(Ft(c)(f.makePeriodicOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency,spec:n.spec})))(br(S(o))(p(k)(function(_){return Tr()()()({frequency:fr(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))},spec:function(v){return Ft(c)(f.setPeriodicOsc({id:s,spec:v}))}})(_)})(r)))))()}})}};return new Ye(a)}}},Di=function(t){return KP(t)};var YP=function(t){return function(e){return function(r){var n=gD(t)(e),a=function(u){return function(f){return qt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(St)(i)(O(N(c))(Ft(c)(f.makePlayBuf({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer,playbackRate:n.playbackRate,bufferOffset:n.bufferOffset,duration:n.duration})))(br(S(o))(p(k)(function(_){return Tr()()()({buffer:function(v){return Ft(c)(f.setBuffer({id:s,buffer:v}))},playbackRate:fr(u.scope)(f)(function(v){return f.setPlaybackRate(function(D){return{id:s,playbackRate:D}}(v))}),bufferOffset:function(v){return Ft(c)(f.setBufferOffset({id:s,bufferOffset:v}))},onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))},duration:function(v){return Ft(c)(f.setDuration({id:s,duration:v}))}})(_)})(r)))))()}})}};return new Ye(a)}}},Qn=function(t){return YP(t)};var ZP=function(t){return function(e){return function(r){var n=PC(t)(e),a=function(u){return function(f){return qt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(St)(i)(O(N(c))(Ft(c)(f.makeSawtoothOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(br(S(o))(p(k)(function(_){return Tr()()()({frequency:fr(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new Ye(a)}}},iE=function(t){return ZP(t)};var tO=function(t){return function(e){return function(r){var n=wC(t)(e),a=function(u){return function(f){return qt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(St)(i)(O(N(c))(Ft(c)(f.makeSinOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(br(S(o))(p(k)(function(_){return Tr()()()({frequency:fr(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new Ye(a)}}},Rf=function(t){return tO(t)},fE=function(t){return function(e){return Rf(t)(e)($(g(c)))}},eO=function(t){return function(e){return function(r){var n=MC(t)(e),a=function(u){return function(f){return qt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(St)(i)(O(N(c))(Ft(c)(f.makeSquareOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(br(S(o))(p(k)(function(_){return Tr()()()({frequency:fr(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new Ye(a)}}},b_=function(t){return eO(t)},cE=function(t){return function(e){return b_(t)(e)($(g(c)))}},rO=function(t){return function(e){return function(r){var n=FC(t)(e),a=function(u){return function(f){return qt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(St)(i)(O(N(c))(Ft(c)(f.makeTriangleOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(br(S(o))(p(k)(function(_){return Tr()()()({frequency:fr(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new Ye(a)}}},ds=function(t){return rO(t)};var nO=function(t){return function(e){return function(r){return function(n){var a=xD(t)(e),u=function(f){return function(i){return qt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(St)(m)(O(N(c))(Ft(c)(i.makeAllpass({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(O(N(c))(br(S(o))(p(k)(function(v){return Tr()()()({frequency:fr(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:fr(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(qr({parent:pn(_),scope:f.scope,raiseId:Pt(Xr(Qr(un)))})(i)(new En(n))))))()}})}};return new Ye(u)}}}},A_=function(t){return function(e){return function(r){return nO(t)(e)($(g(c)))(r)}}},WD=function(t){return function(e){return function(r){return function(n){var a=TD(t)(e),u=function(f){return function(i){return qt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(St)(m)(O(N(c))(Ft(c)(i.makeBandpass({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(O(N(c))(br(S(o))(p(k)(function(v){return Tr()()()({frequency:fr(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:fr(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(qr({parent:pn(_),scope:f.scope,raiseId:Pt(Xr(Qr(un)))})(i)(new En(n))))))()}})}};return new Ye(u)}}}},hn=function(t){return function(e){return function(r){return WD(t)(e)($(g(c)))(r)}}},y_=function(t){return function(e){return function(r){return function(n){var a=hD(t)(e),u=function(f){return function(i){return qt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(St)(m)(O(N(c))(Ft(c)(i.makeDelay({id:_,parent:f.parent,scope:f.scope,delayTime:a.delayTime,maxDelayTime:a.maxDelayTime})))(O(N(c))(br(S(o))(p(k)(function(v){return Tr()()()({delayTime:fr(f.scope)(i)(function(D){return i.setDelay(function(l){return{id:_,delayTime:l}}(D))})})(v)})(r)))(qr({parent:pn(_),scope:f.scope,raiseId:Pt(Xr(Qr(un)))})(i)(new En(n))))))()}})}};return new Ye(u)}}}},yo=function(t){return function(e){return function(r){return y_(t)(e)($(g(c)))(r)}}},aO=function(t){return function(e){return function(r){return function(n){var a=qC(t)(e),u=function(f){return function(i){return qt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(St)(m)(O(N(c))(Ft(c)(i.makeDynamicsCompressor({id:_,parent:f.parent,scope:f.scope,threshold:a.threshold,ratio:a.ratio,knee:a.knee,attack:a.attack,release:a.release})))(O(N(c))(br(S(o))(p(k)(function(v){return Tr()()()({threshold:fr(f.scope)(i)(function(D){return i.setThreshold(function(l){return{id:_,threshold:l}}(D))}),ratio:fr(f.scope)(i)(function(D){return i.setRatio(function(l){return{id:_,ratio:l}}(D))}),knee:fr(f.scope)(i)(function(D){return i.setKnee(function(l){return{id:_,knee:l}}(D))}),attack:fr(f.scope)(i)(function(D){return i.setAttack(function(l){return{id:_,attack:l}}(D))}),release:fr(f.scope)(i)(function(D){return i.setRelease(function(l){return{id:_,release:l}}(D))})})(v)})(r)))(qr({parent:pn(_),scope:f.scope,raiseId:Pt(Xr(Qr(un)))})(i)(new En(n))))))()}})}};return new Ye(u)}}}},lE=function(t){return function(e){return aO(t)(e)($(g(c)))}},uO=function(){return function(t){return function(e){return function(r){return function(n){var a=function(u){return function(f){return qt(function(i){return function(){var s=OD(p(xe)(T(""))(df()(r)))(),_=Ve(wt)(g(c))(ro(ni)(function(){var Gt=function(re){return function(he){var gr=re,lt=!1,oe;function ke(Bt,vr){if(vr instanceof Ye)return lt=!0,vr.value0({parent:pn("@portal@"),scope:e(u.scope),raiseId:function(Su){return ID(Bt)(Su)(s)}})(f);gr=Bt,he=Ot(it)(1)([vr])}for(;!lt;)oe=ke(gr,he);return oe}};return Gt}())(df()(r))),v=St(_)(i)(),D=Rt(o)(Br(h(c)(void 0)))(),l=p(x)(ft)(RD(s))(),C=p(Um)(function(Gt){return new Ye(function(re){return function(he){return qt(function(gr){return function(){return re.raiseId(Gt)(),ea(c)(r_)(re.parent)(function(oe){return gr(he.connectXToY({from:Gt,to:oe}))})(),h(c)(void 0)}})}})})(l),ot=qr(u)(f)(n(C)(ft)),vt=St(ot)(i)();return Kt(x)(Rt(o)(an(vt)(D)))(),function(){return v(),$n(c)(!t)(ea(c)(wt)(df()(l))(function(re){return i(f.deleteFromCache({id:re}))}))(),Zn(hr)(Rt(o)($r(D)))()}}})}};return new Ye(a)}}}}},oO=function(){return function(t){return uO()(!1)(Z(et))(t)}},$a=function(t){return function(e){return oO()(hy(t))(oo(Ta)(Ty()()()()()({reflectType:function(){return 0}})(d.value))(e))}};var GD=function(t){return function(e){return function(r){return function(n){var a=SD(t)(e),u=function(f){return function(i){return qt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(St)(m)(O(N(c))(Ft(c)(i.makeHighpass({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(O(N(c))(br(S(o))(p(k)(function(v){return Tr()()()({frequency:fr(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:fr(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(qr({parent:pn(_),scope:f.scope,raiseId:Pt(Xr(Qr(un)))})(i)(new En(n))))))()}})}};return new Ye(u)}}}},Fc=function(t){return function(e){return function(r){return GD(t)(e)($(g(c)))(r)}}},iO=function(t){return function(e){return function(r){return function(n){var a=BC(t)(e),u=function(f){return function(i){return qt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(St)(m)(O(N(c))(Ft(c)(i.makeHighshelf({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,gain:a.gain})))(O(N(c))(br(S(o))(p(k)(function(v){return Tr()()()({frequency:fr(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),gain:fr(f.scope)(i)(function(D){return i.setGain(function(l){return{id:_,gain:l}}(D))})})(v)})(r)))(qr({parent:pn(_),scope:f.scope,raiseId:Pt(Xr(Qr(un)))})(i)(new En(n))))))()}})}};return new Ye(u)}}}},_E=function(t){return function(e){return function(r){return iO(t)(e)($(g(c)))(r)}}},pE=function(t){return function(e){return function(r){return function(n){var a=CD(t)(e),u=function(f){return function(i){return qt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(St)(m)(O(N(c))(Ft(c)(i.makeLowpass({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(O(N(c))(br(S(o))(p(k)(function(v){return Tr()()()({frequency:fr(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:fr(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(qr({parent:pn(_),scope:f.scope,raiseId:Pt(Xr(Qr(un)))})(i)(new En(n))))))()}})}};return new Ye(u)}}}},$c=function(t){return function(e){return function(r){return pE(t)(e)($(g(c)))(r)}}},fO=function(t){return function(e){return function(r){return function(n){var a=WC(t)(e),u=function(f){return function(i){return qt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(St)(m)(O(N(c))(Ft(c)(i.makeLowshelf({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,gain:a.gain})))(O(N(c))(br(S(o))(p(k)(function(v){return Tr()()()({frequency:fr(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),gain:fr(f.scope)(i)(function(D){return i.setGain(function(l){return{id:_,gain:l}}(D))})})(v)})(r)))(qr({parent:pn(_),scope:f.scope,raiseId:Pt(Xr(Qr(un)))})(i)(new En(n))))))()}})}};return new Ye(u)}}}},sE=function(t){return function(e){return function(r){return fO(t)(e)($(g(c)))(r)}}},cO=function(t){return function(e){return function(r){return function(n){var a=NC(t)(e),u=function(f){return function(i){return qt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(St)(m)(O(N(c))(Ft(c)(i.makeNotch({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(O(N(c))(br(S(o))(p(k)(function(v){return Tr()()()({frequency:fr(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:fr(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(qr({parent:pn(_),scope:f.scope,raiseId:Pt(Xr(Qr(un)))})(i)(new En(n))))))()}})}};return new Ye(u)}}}},Mc=function(t){return function(e){return function(r){return cO(t)(e)($(g(c)))(r)}}},lO=function(t){return function(e){return function(r){return function(n){var a=$C(t)(e),u=function(f){return function(i){return qt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(St)(m)(O(N(c))(Ft(c)(i.makeStereoPanner({id:_,parent:f.parent,scope:f.scope,pan:a.pan})))(O(N(c))(br(S(o))(p(k)(function(v){return Tr()()()({pan:fr(f.scope)(i)(function(D){return i.setPan(function(l){return{id:_,pan:l}}(D))})})(v)})(r)))(qr({parent:pn(_),scope:f.scope,raiseId:Pt(Xr(Qr(un)))})(i)(new En(n))))))()}})}};return new Ye(u)}}}},mE=function(t){return function(e){return lO(t)(e)($(g(c)))}},_O=function(t){return function(e){return function(r){return function(n){var a=RC(t)(e),u=function(f){return function(i){return qt(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(St)(m)(O(N(c))(Ft(c)(i.makePeaking({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q,gain:a.gain})))(O(N(c))(br(S(o))(p(k)(function(v){return Tr()()()({frequency:fr(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:fr(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))}),gain:fr(f.scope)(i)(function(D){return i.setGain(function(l){return{id:_,gain:l}}(D))})})(v)})(r)))(qr({parent:pn(_),scope:f.scope,raiseId:Pt(Xr(Qr(un)))})(i)(new En(n))))))()}})}};return new Ye(u)}}}},wc=function(t){return function(e){return function(r){return _O(t)(e)($(g(c)))(r)}}},vE=function(t){return function(e){return function(r){var n=kD(t)(e),a=function(u){return function(f){return qt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(St)(i)(O(N(c))(Ft(c)(f.makeWaveShaper({id:s,parent:u.parent,scope:u.scope,curve:n.curve,oversample:n.oversample})))(qr({parent:pn(s),scope:u.scope,raiseId:Pt(Xr(Qr(un)))})(f)(new En(r)))))()}})}};return new Ye(a)}}},pO=function(t){return function(e){return function(r){var n=zC(t)(e),a=function(u){return function(f){return qt(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(St)(i)(O(N(c))(Ft(c)(f.makeConstant({id:s,parent:u.parent,scope:u.scope,offset:n.offset})))(br(S(o))(p(k)(function(_){return Tr()()()({offset:fr(u.scope)(f)(function(v){return f.setOffset(function(D){return{id:s,offset:D}}(v))}),onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new Ye(a)}}},bs=function(t){return pO(t)};function BD(){window.scrollTo(0,0)}var ko=function(t){return t.sequential},Nn=function(t){return t.parallel};var Tn=function(t){return function(e){return function(r){return new U(J(t)("button")(e)(new H(r)))}}};var Ma=function(){var t={},e="Pure",r="Throw",n="Catch",a="Sync",u="Async",f="Bind",i="Bracket",m="Fork",s="Sequential",_="Map",v="Apply",D="Alt",l="Cons",C="Resume",ot="Release",vt="Finalizer",Gt="Finalized",re="Forked",he="Fiber",gr="Thunk";function lt(Ht,je,zr,cr){this.tag=Ht,this._1=je,this._2=zr,this._3=cr}function oe(Ht){var je=function(zr,cr,ie){return new lt(Ht,zr,cr,ie)};return je.tag=Ht,je}function ke(Ht){return new lt(e,void 0)}function Bt(Ht){try{Ht()}catch(je){setTimeout(function(){throw je},0)}}function vr(Ht,je,zr){try{return je(zr())}catch(cr){return Ht(cr)}}function Su(Ht,je,zr){try{return je(zr)()}catch(cr){return zr(Ht(cr))(),ke}}var Vu=function(){var Ht=1024,je=0,zr=0,cr=new Array(Ht),ie=!1;function Et(){var er;for(ie=!0;je!==0;)je--,er=cr[zr],cr[zr]=void 0,zr=(zr+1)%Ht,er();ie=!1}return{isDraining:function(){return ie},enqueue:function(er){var Oe,Wr;je===Ht&&(Wr=ie,Et(),ie=Wr),cr[(zr+je)%Ht]=er,je++,ie||Et()}}}();function xi(Ht){var je={},zr=0,cr=0;return{register:function(ie){var Et=zr++;ie.onComplete({rethrow:!0,handler:function(er){return function(){cr--,delete je[Et]}}})(),je[Et]=ie,cr++},isEmpty:function(){return cr===0},killAll:function(ie,Et){return function(){if(cr===0)return Et();var er=0,Oe={};function Wr(Dr){Oe[Dr]=je[Dr].kill(ie,function(rn){return function(){delete Oe[Dr],er--,Ht.isLeft(rn)&&Ht.fromLeft(rn)&&setTimeout(function(){throw Ht.fromLeft(rn)},0),er===0&&Et()}})()}for(var Kn in je)je.hasOwnProperty(Kn)&&(er++,Wr(Kn));return je={},zr=0,cr=0,function(Dr){return new lt(a,function(){for(var rn in Oe)Oe.hasOwnProperty(rn)&&Oe[rn]()})}}}}}var au=0,Dn=1,Xo=2,Uf=3,qf=4,Fr=5,Qo=6;function Hf(Ht,je,zr){var cr=0,ie=au,Et=zr,er=null,Oe=null,Wr=null,Kn=null,Dr=null,rn=0,uf=0,hu=null,Fi=!0;function $i(fe){for(var _e,Ue,ze;;)switch(_e=null,Ue=null,ze=null,ie){case Xo:ie=Dn;try{Et=Wr(Et),Kn===null?Wr=null:(Wr=Kn._1,Kn=Kn._2)}catch(fa){ie=Fr,er=Ht.left(fa),Et=null}break;case Uf:Ht.isLeft(Et)?(ie=Fr,er=Et,Et=null):Wr===null?ie=Fr:(ie=Xo,Et=Ht.fromRight(Et));break;case Dn:switch(Et.tag){case f:Wr&&(Kn=new lt(l,Wr,Kn)),Wr=Et._2,ie=Dn,Et=Et._1;break;case e:Wr===null?(ie=Fr,Et=Ht.right(Et._1)):(ie=Xo,Et=Et._1);break;case a:ie=Uf,Et=vr(Ht.left,Ht.right,Et._1);break;case u:ie=qf,Et=Su(Ht.left,Et._1,function(fa){return function(){cr===fe&&(cr++,Vu.enqueue(function(){cr===fe+1&&(ie=Uf,Et=fa,$i(cr))}))}});return;case r:ie=Fr,er=Ht.left(Et._1),Et=null;break;case n:Wr===null?Dr=new lt(l,Et,Dr,Oe):Dr=new lt(l,Et,new lt(l,new lt(C,Wr,Kn),Dr,Oe),Oe),Wr=null,Kn=null,ie=Dn,Et=Et._1;break;case i:rn++,Wr===null?Dr=new lt(l,Et,Dr,Oe):Dr=new lt(l,Et,new lt(l,new lt(C,Wr,Kn),Dr,Oe),Oe),Wr=null,Kn=null,ie=Dn,Et=Et._1;break;case m:ie=Uf,_e=Hf(Ht,je,Et._2),je&&je.register(_e),Et._1&&_e.run(),Et=Ht.right(_e);break;case s:ie=Dn,Et=a0(Ht,je,Et._1);break}break;case Fr:if(Wr=null,Kn=null,Dr===null)ie=Qo,Et=Oe||er||Et;else switch(_e=Dr._3,ze=Dr._1,Dr=Dr._2,ze.tag){case n:Oe&&Oe!==_e&&rn===0?ie=Fr:er&&(ie=Dn,Et=ze._2(Ht.fromLeft(er)),er=null);break;case C:Oe&&Oe!==_e&&rn===0||er?ie=Fr:(Wr=ze._1,Kn=ze._2,ie=Xo,Et=Ht.fromRight(Et));break;case i:rn--,er===null&&(Ue=Ht.fromRight(Et),Dr=new lt(l,new lt(ot,ze._2,Ue),Dr,_e),(Oe===_e||rn>0)&&(ie=Dn,Et=ze._3(Ue)));break;case ot:Dr=new lt(l,new lt(Gt,Et,er),Dr,Oe),ie=Dn,Oe&&Oe!==_e&&rn===0?Et=ze._1.killed(Ht.fromLeft(Oe))(ze._2):er?Et=ze._1.failed(Ht.fromLeft(er))(ze._2):Et=ze._1.completed(Ht.fromRight(Et))(ze._2),er=null,rn++;break;case vt:rn++,Dr=new lt(l,new lt(Gt,Et,er),Dr,Oe),ie=Dn,Et=ze._1;break;case Gt:rn--,ie=Fr,Et=ze._1,er=ze._2;break}break;case Qo:for(var Jr in hu)hu.hasOwnProperty(Jr)&&(Fi=Fi&&hu[Jr].rethrow,Bt(hu[Jr].handler(Et)));hu=null,Oe&&er?setTimeout(function(){throw Ht.fromLeft(er)},0):Ht.isLeft(Et)&&Fi&&setTimeout(function(){if(Fi)throw Ht.fromLeft(Et)},0);return;case au:ie=Dn;break;case qf:return}}function Vr(fe){return function(){if(ie===Qo)return Fi=Fi&&fe.rethrow,fe.handler(Et)(),function(){};var _e=uf++;return hu=hu||{},hu[_e]=fe,function(){hu!==null&&delete hu[_e]}}}function de(fe,_e){return function(){if(ie===Qo)return _e(Ht.right(void 0))(),function(){};var Ue=Vr({rethrow:!1,handler:function(){return _e(Ht.right(void 0))}})();switch(ie){case au:Oe=Ht.left(fe),ie=Qo,Et=Oe,$i(cr);break;case qf:Oe===null&&(Oe=Ht.left(fe)),rn===0&&(ie===qf&&(Dr=new lt(l,new lt(vt,Et(fe)),Dr,Oe)),ie=Fr,Et=null,er=null,$i(++cr));break;default:Oe===null&&(Oe=Ht.left(fe)),rn===0&&(ie=Fr,Et=null,er=null)}return Ue}}function Re(fe){return function(){var _e=Vr({rethrow:!1,handler:fe})();return ie===au&&$i(cr),_e}}return{kill:de,join:Re,onComplete:Vr,isSuspended:function(){return ie===au},run:function(){ie===au&&(Vu.isDraining()?$i(cr):Vu.enqueue(function(){$i(cr)}))}}}function Ko(Ht,je,zr,cr){var ie=0,Et={},er=0,Oe={},Wr=new Error("[ParAff] Early exit"),Kn=null,Dr=t;function rn(Vr,de,Re){var fe=de,_e=null,Ue=null,ze=0,Jr={},fa,Zc;t:for(;;)switch(fa=null,fe.tag){case re:if(fe._3===t&&(fa=Et[fe._1],Jr[ze++]=fa.kill(Vr,function(u0){return function(){ze--,ze===0&&Re(u0)()}})),_e===null)break t;fe=_e._2,Ue===null?_e=null:(_e=Ue._1,Ue=Ue._2);break;case _:fe=fe._2;break;case v:case D:_e&&(Ue=new lt(l,_e,Ue)),_e=fe,fe=fe._1;break}if(ze===0)Re(Ht.right(void 0))();else for(Zc=0,fa=ze;Zc<fa;Zc++)Jr[Zc]=Jr[Zc]();return Jr}function uf(Vr,de,Re){var fe,_e,Ue,ze,Jr,fa;Ht.isLeft(Vr)?(fe=Vr,_e=null):(_e=Vr,fe=null);t:for(;;){if(Ue=null,ze=null,Jr=null,fa=null,Kn!==null)return;if(de===null){cr(fe||_e)();return}if(de._3!==t)return;switch(de.tag){case _:fe===null?(de._3=Ht.right(de._1(Ht.fromRight(_e))),_e=de._3):de._3=fe;break;case v:if(Ue=de._1._3,ze=de._2._3,fe){if(de._3=fe,Jr=!0,fa=er++,Oe[fa]=rn(Wr,fe===Ue?de._2:de._1,function(){return function(){delete Oe[fa],Jr?Jr=!1:Re===null?uf(fe,null,null):uf(fe,Re._1,Re._2)}}),Jr){Jr=!1;return}}else{if(Ue===t||ze===t)return;_e=Ht.right(Ht.fromRight(Ue)(Ht.fromRight(ze))),de._3=_e}break;case D:if(Ue=de._1._3,ze=de._2._3,Ue===t&&Ht.isLeft(ze)||ze===t&&Ht.isLeft(Ue))return;if(Ue!==t&&Ht.isLeft(Ue)&&ze!==t&&Ht.isLeft(ze))fe=_e===Ue?ze:Ue,_e=null,de._3=fe;else if(de._3=_e,Jr=!0,fa=er++,Oe[fa]=rn(Wr,_e===Ue?de._2:de._1,function(){return function(){delete Oe[fa],Jr?Jr=!1:Re===null?uf(_e,null,null):uf(_e,Re._1,Re._2)}}),Jr){Jr=!1;return}break}Re===null?de=null:(de=Re._1,Re=Re._2)}}function hu(Vr){return function(de){return function(){delete Et[Vr._1],Vr._3=de,uf(de,Vr._2._1,Vr._2._2)}}}function Fi(){var Vr=Dn,de=zr,Re=null,fe=null,_e,Ue;t:for(;;)switch(_e=null,Ue=null,Vr){case Dn:switch(de.tag){case _:Re&&(fe=new lt(l,Re,fe)),Re=new lt(_,de._1,t,t),de=de._2;break;case v:Re&&(fe=new lt(l,Re,fe)),Re=new lt(v,t,de._2,t),de=de._1;break;case D:Re&&(fe=new lt(l,Re,fe)),Re=new lt(D,t,de._2,t),de=de._1;break;default:Ue=ie++,Vr=Fr,_e=de,de=new lt(re,Ue,new lt(l,Re,fe),t),_e=Hf(Ht,je,_e),_e.onComplete({rethrow:!1,handler:hu(de)})(),Et[Ue]=_e,je&&je.register(_e)}break;case Fr:if(Re===null)break t;Re._1===t?(Re._1=de,Vr=Dn,de=Re._2,Re._2=t):(Re._2=de,de=Re,fe===null?Re=null:(Re=fe._1,fe=fe._2))}for(Dr=de,Ue=0;Ue<ie;Ue++)Et[Ue].run()}function $i(Vr,de){Kn=Ht.left(Vr);var Re;for(var fe in Oe)if(Oe.hasOwnProperty(fe)){Re=Oe[fe];for(fe in Re)Re.hasOwnProperty(fe)&&Re[fe]()}Oe=null;var _e=rn(Vr,Dr,de);return function(Ue){return new lt(u,function(ze){return function(){for(var Jr in _e)_e.hasOwnProperty(Jr)&&_e[Jr]();return ke}})}}return Fi(),function(Vr){return new lt(u,function(de){return function(){return $i(Vr,de)}})}}function a0(Ht,je,zr){return new lt(u,function(cr){return function(){return Ko(Ht,je,zr,cr)}})}return lt.EMPTY=t,lt.Pure=oe(e),lt.Throw=oe(r),lt.Catch=oe(n),lt.Sync=oe(a),lt.Async=oe(u),lt.Bind=oe(f),lt.Bracket=oe(i),lt.Fork=oe(m),lt.Seq=oe(s),lt.ParMap=oe(_),lt.ParApply=oe(v),lt.ParAlt=oe(D),lt.Fiber=Hf,lt.Supervisor=xi,lt.Scheduler=Vu,lt.nonCanceler=ke,lt}(),DE=Ma.Pure,yO=Ma.Throw;function dE(t){return function(e){return e.tag===Ma.Pure.tag?Ma.Pure(t(e._1)):Ma.Bind(e,function(r){return Ma.Pure(t(r))})}}function bE(t){return function(e){return Ma.Bind(t,e)}}var AE=Ma.Sync;function yE(t){return function(e){return Ma.ParMap(t,e)}}function kE(t){return function(e){return Ma.ParApply(t,e)}}function gE(t){return function(e){return Ma.ParAlt(t,e)}}var Pc=Ma.Async;function CE(t,e){return function(){return Ma.Fiber(t,null,e)}}var kO=function(){function t(r,n){return r===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,r)}function e(r,n){return r===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(r,n){return Ma.Async(function(a){return function(){var u=t(n,a(r()));return function(){return Ma.Sync(function(){return r(e(n,u))})}}})}}(),EE=Ma.Seq;var CO=function(t){return function(e){return function(r){var n=ko(t),a=nr(t.Applicative1())(e)(function(){var u=Nn(t);return function(f){return u(r(f))}}());return function(u){return n(a(u))}}}},SE=function(t){return function(e){return function(r){var n=ko(t),a=ra(e)(t.Applicative1())(function(){var u=Nn(t);return function(f){return u(r(f))}}());return function(u){return n(a(u))}}}},hE=function(t){return function(e){return CO(t)(e)(Z(et))}};var EO=function(t){return t};var xE=function(t){return t};var g_=function(t){return t.toDuration};var FE={fromDuration:Dm()()(EO)(function(t){return t*1e3}),toDuration:Dm()()(xE)(function(t){return t/1e3})};var $E=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}};var hO=function(t){return t};var Ic={map:yE},di={map:dE};var TO=function(){var t=function(n){if(n instanceof Zt)return n.value0;if(n instanceof Yt)return pu("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): "+[n.constructor.name])},e=function(n){if(n instanceof Yt)return n.value0;if(n instanceof Zt)return pu("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): "+[n.constructor.name])},r=function(n){if(n instanceof Yt)return!0;if(n instanceof Zt)return!1;throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): "+[n.constructor.name])};return{isLeft:r,fromLeft:e,fromRight:t,left:Yt.create,right:Zt.create}}(),xO=function(t){return CE(TO,t)},go=function(t){return function(){var r=xO(t)();return r.run(),r}},zo=function(){var t=Kt(x);return function(e){return t(go(e))}}();var bi={apply:kE,Functor0:function(){return Ic}};var UD={Applicative0:function(){return ba},Bind1:function(){return Rr}},Rr={bind:bE,Apply0:function(){return qD(0)}},ba={pure:DE,Apply0:function(){return qD(0)}},qD=$E("applyAff","Effect.Aff",function(){return{apply:ju(UD),Functor0:function(){return di}}}),ME=qD(71);var Ir={liftEffect:AE,Monad0:function(){return UD}},wE=function(){var t=mr(Ir);return function(e){return hO(T(t(e)))}}(),PE=function(t){return Pc(function(e){return p(x)(wE)(t.join(e))})};var OE=function(t){return function(e){return q(Rr)(mr(Ir)(e.isSuspended))(function(r){return r?mr(Ir)(Kt(x)(e.kill(t,T(h(c)(void 0))))):Pc(function(n){return p(x)(wE)(e.kill(t,n))})})}};var Ln={parallel:ft,sequential:EE,Monad0:function(){return UD},Applicative1:function(){return FO(0)}},FO=$E("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=Nn(Ln),e=h(ba);return function(r){return t(e(r))}}(),Apply0:function(){return bi}}});var $O={append:function(t){return function(e){return function(r){return hE(Ln)(wt)([t(r),e(r)])}}}};var MO=T(h(ba)(void 0)),IE={mempty:MO,Semigroup0:function(){return $O}};var RE={alt:gE,Functor0:function(){return Ic}};var NE=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),C_=function(){function t(){}return t.value=new t,t}(),Nf=function(){function t(){}return t.value=new t,t}(),E_=function(){function t(){}return t.value=new t,t}(),Lf=function(){function t(){}return t.value=new t,t}(),S_=function(){function t(){}return t.value=new t,t}(),h_=function(){function t(){}return t.value=new t,t}(),LE=function(){function t(){}return t.value=new t,t}(),As=function(){function t(){}return t.value=new t,t}(),ys=function(){function t(){}return t.value=new t,t}(),T_=function(){function t(){}return t.value=new t,t}(),x_=function(){function t(){}return t.value=new t,t}(),WE=function(){function t(){}return t.value=new t,t}(),Rc=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),HD=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var wO="numeric",PO="sudden",OO="unit",IO="cancel",RO="step",NO="linear",LO="exponential",WO="envelope",GE=function(t,e,r,n){if(r.type===PO)t.value=r.value.n;else if(r.type===OO)e.id&&BO(e.id,n),n.units[r.value.i].main.connect(t),e.id=r.value.i;else if(r.type===wO)t[r.value.t.type===RO?"setValueAtTime":r.value.t.type===NO?"linearRampToValueAtTime":r.value.t.type===LO?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](r.value.n,r.value.o);else if(r.type===IO)r.value.hold?t.cancelAndHoldAtTime(r.value.o):t.cancelScheduledValues(r.value.o);else if(r.type===WO){let a=r.value.o;t.cancelScheduledValues(Math.max(0,a)),t.setValueCurveAtTime(r.value.p,a,r.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(r))},GO=function(t,e,r,n,a){return n[r]||(n[r]={}),GE(e.parameters.get(r),n[r],a,t)},qu=function(t,e,r,n,a){return n[r]||(n[r]={}),GE(e[r],n[r],a,t)},Ar=function(t,e,r){r.scopes[e]||(r.scopes[e]=[]),r.scopes[e].push(t),r.units[t].scope=e},yr=function(t,e){e.toConnect[t]&&(e.toConnect[t].forEach(function(r){r.w?e.units[r.w]?r.f():(e.toConnect[r.w]||(e.toConnect[r.w]=[]),e.toConnect[r.w].push({f:r.f})):r.f()}),delete e.toConnect[t])},kr=function(t,e,r){e.type==="just"&&BE(t,e.value,r)},BE=function(t,e,r){var n=function(){r.units[t].audioOutgoing.push(e),r.units[t].pendingOn||(r.units[t].main.connect(r.units[e].main),r.units[e].se&&r.units[t].main.connect(r.units[e].se))};if(!r.units[t]){r.toConnect[t]||(r.toConnect[t]=[]);var a={f:n};e!==t&&!r.units[e]&&(a.w=e),r.toConnect[t].push(a);return}if(!r.units[e]){r.toConnect[e]||(r.toConnect[e]=[]);var a={f:n};e!==t&&!r.units[t]&&(a.w=t),r.toConnect[e].push(a);return}n()};function zD(t){return function(e){return function(){delete e.units[t.id]}}}function VD(t){return function(e){return function(){BE(t.from,t.to,e)}}}var BO=function(t,e){if(e.units[t].scope==="@fan@")return;let r=e.units[t].scope;e.scopes[r].forEach(n=>{delete e.units[n]}),delete e.scopes[r]};function JD(t){return function(e){return function(){var r=t.from,n=t.to;if(e.units[r].audioOutgoing=e.units[r].audioOutgoing.filter(function(u){return u!==n}),e.units[r].main.disconnect(e.units[n].main),e.units[n].se&&e.units[r].main.disconnect(e.units[n].se),e.units[r].scope==="@fan@")return;let a=e.units[r].scope;e.scopes[a].forEach(u=>{delete e.units[u]}),delete e.scopes[a]}}}function jD(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"allpass",Q:t.q,frequency:t.frequency})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function XD(t){return function(e){return function(){var r=t.id,n=t.cb,a=new AnalyserNode(e.context,t),u=n(a)();e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:n,analyser:u,main:e.context.createGain(),se:a},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function QD(t){return function(e){return function(){var r=t.id,n=t.options;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(e.context,n.name,{numberOfInputs:n.numberOfInputs,numberOfOutputs:n.numberOfOutputs,outputChannelCount:n.outputChannelCount,parameterData:n.parameterData,processorOptions:n.processorOptions})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function KD(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"bandpass",Q:t.q,frequency:t.frequency})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function YD(t){return function(e){return function(){var r=t.id,n=function(u,f){return new ConstantSourceNode(u,f)},a={offset:t.offset};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function ZD(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(e.context,{buffer:t.buffer})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function td(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(e.context,{delayTime:t.delayTime,maxDelayTime:t.maxDelayTime})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function ed(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(e.context,{knee:t.knee,ratio:t.ratio,threshold:t.threshold,attack:t.attack,release:t.release})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}var rd=function(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(e.context,{gain:t.gain})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}};function nd(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highpass",Q:t.q,frequency:t.frequency})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function ad(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highshelf",frequency:t.frequency,gain:t.gain})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function ud(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(e.context,{feedforward:t.feedforward,feedback:t.feedback})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function od(t){return function(e){return function(){var r=t.id,n=function(u,f){return new AudioBufferSourceNode(u,f)},a={loop:!0,buffer:t.buffer,loopStart:t.loopStart,loopEnd:t.loopEnd,playbackRate:t.playbackRate};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function id(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowpass",Q:t.q,frequency:t.frequency})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function fd(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowshelf",frequency:t.frequency,gain:t.gain})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function cd(t){return function(e){return function(){var r=t.id,n=t.element,a=function(){var u=e.context.createMediaElementSource(n);return u};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:a,resumeClosure:{},main:a()},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function ld(t){return function(e){return function(){var r=t.id;e.units[t.id]={main:e.context.createMediaStreamSource(t.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function _d(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"notch",frequency:t.frequency,Q:t.q})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function pd(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"peaking",frequency:t.frequency,Q:t.q,gain:t.gain})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function sd(t){return function(e){return function(){var r=t.id,n=function(u,f){var i={frequency:f.frequency,periodicWave:f.spec.type==="wave"?f.spec.value:Kd(e.context)(f.spec.value.real)(f.spec.value.img)()},m=new OscillatorNode(u,i);return m},a={frequency:t.frequency,type:"custom",spec:t.spec};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function md(t){return function(e){return function(){var r=t.id,n=function(u,f){var i={loop:f.loop,buffer:f.buffer,playbackRate:f.playbackRate};return new AudioBufferSourceNode(u,i)},a={loop:!1,buffer:t.buffer,playbackRate:t.playbackRate,bufferOffset:t.bufferOffset,duration:t.duration};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function vd(t){return function(e){return function(){var r=t.id,n=t.cb,a=e.context.createMediaStreamDestination(),u=new MediaRecorder(a.stream);n(u)(),u.start(),e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:n,recorder:u,main:e.context.createGain(),se:a},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function Dd(t){return function(e){return function(){var r=t.id,n=function(u,f){return new OscillatorNode(u,f)},a={frequency:t.frequency,type:"sawtooth"};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function dd(t){return function(e){return function(){var r=t.id,n=function(u,f){return new OscillatorNode(u,f)},a={frequency:t.frequency,type:"sine"};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function bd(t){return function(e){return function(){e.units[t.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:e.context.createGain(),se:e.context.destination}}}}function Ad(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(e.context,{pan:t.pan})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function yd(t){return function(e){return function(){var r=t.id,n=function(u,f){return new OscillatorNode(u,f)},a={frequency:t.frequency,type:"square"};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function kd(t){return function(e){return function(){var r=t.id,n=function(u,f){return new OscillatorNode(u,f)},a={frequency:t.frequency,type:"triangle"};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function gd(t){return function(e){return function(){var r=t.id,n=t.curve,a=t.oversample;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(e.context,{curve:n,oversample:a.type})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function Cd(t){return function(e){return function(){var r=t.id,n=t.cb;e.units[r].analyserOrig!==n&&(e.units[r].analyser&&e.units[r].analyser(),e.units[r].analyser=n(e.units[r].se)(),e.units[r].analyserOrig=n)}}}function Ed(t){return function(e){return function(){var r=t.cb,n=t.id;if(e.units[n].recorderOrig!==r){e.units[n].recorder&&e.units[n].recorder.stop();var a=r;e.units[n].recorderOrig=r;var u=new MediaRecorder(e.units[n].se);a(u)(),u.start()}}}}function Sd(t){return function(e){return function(){var r=t.id,n=t.curve;e.units[r].main.curve=n}}}function hd(t){return function(e){return function(){var r=t.id,n=t.paramName,a=t.paramValue;GO(e,e.units[r].main,n,e.units[r].controllers,a)}}}var Hu=function(t,e,r){e.resume&&t.value.n!==void 0&&(e.resume[r]=t.value.n)};function Td(t){return function(e){return function(){var r=t.id,n=t.gain;qu(e,e.units[r].main,"gain",e.units[r].controllers,n),Hu(n,e.units[r],"gain")}}}function xd(t){return function(e){return function(){var r=t.id,n=t.q;qu(e,e.units[r].main,"Q",e.units[r].controllers,n),Hu(n,e.units[r],"Q")}}}function Fd(t){return function(e){return function(){var r=t.id,n=t.buffer;e.units[r].resume&&(e.units[r].resume.buffer=n)}}}function $d(t){return function(e){return function(){var r=t.id,n=t.buffer;e.units[r].main.buffer=n}}}function Md(t){return function(e){return function(){var r=t.id,n=t.spec;e.units[r].resume&&(e.units[r].resume.spec=n)}}}function wd(t){return function(e){return function(){var r=t.id,n=t.pan;qu(e,e.units[r].main,"pan",e.units[r].controllers,n),Hu(n,e.units[r],"pan")}}}function Pd(t){return function(e){return function(){var r=t.id,n=t.threshold;qu(e,e.units[r].main,"threshold",e.units[r].controllers,n),Hu(n,e.units[r],"threshold")}}}function Od(t){return function(e){return function(){var r=t.id,n=t.loopStart;e.units[r].main.loopStart=n,e.units[r].resume.loopStart=n}}}function Id(t){return function(e){return function(){var r=t.id,n=t.loopEnd;e.units[r].main.loopEnd=n,e.units[r].resume.loopEnd=n}}}function Rd(t){return function(e){return function(){var r=t.id,n=t.bufferOffset;e.units[r].resume.bufferOffset=n}}}function Nd(t){return function(e){return function(){var r=t.id,n=t.duration;e.units[r].duration=n}}}function Ld(t){return function(e){return function(){var r=t.id,n=t.release;qu(e,e.units[r].main,"release",e.units[r].controllers,n),Hu(n,e.units[r],"release")}}}function Wd(t){return function(e){return function(){var r=t.id,n=t.offset;qu(e,e.units[r].main,"offset",e.units[r].controllers,n),Hu(n,e.units[r],"offset")}}}function Gd(t){return function(e){return function(){var r=t.id,n=t.ratio;qu(e,e.units[r].main,"ratio",e.units[r].controllers,n),Hu(n,e.units[r],"ratio")}}}function Bd(t){return function(e){return function(){var r=t.id,n=t.attack;qu(e,e.units[r].main,"attack",e.units[r].controllers,n),Hu(n,e.units[r],"attack")}}}function Ud(t){return function(e){return function(){var r=t.id,n=t.knee;qu(e,e.units[r].main,"knee",e.units[r].controllers,n),Hu(n,e.units[r],"knee")}}}function qd(t){return function(e){return function(){var r=t.id,n=t.delayTime;qu(e,e.units[r].main,"delayTime",e.units[r].controllers,n),Hu(n,e.units[r],"delayTime")}}}function Hd(t){return function(e){return function(){var r=t.id,n=t.playbackRate;qu(e,e.units[r].main,"playbackRate",e.units[r].controllers,n),Hu(n,e.units[r],"playbackRate")}}}function zd(t){return function(e){return function(){var r=t.id,n=t.frequency;qu(e,e.units[r].main,"frequency",e.units[r].controllers,n),Hu(n,e.units[r],"frequency")}}}function Vd(t){return function(e){return function(){var r=t.id,n=t.onOff;n.x.type==="on"?UO(r)(n)(e)():n.x.type==="off"&&qO(r)(n)(e)()}}}var UO=function(t){return function(e){return function(r){return function(){if(!r.units[t].onOff){r.units[t].pendingOn=!1,r.units[t].onOff=!0,r.units[t].main=r.units[t].createClosure(r.context,r.units[t].resume);for(var n=0;n<r.units[t].audioOutgoing.length;n++){var a=r.units[t].audioOutgoing[n];r.units[t].main.connect(r.units[a].main),r.units[a].se&&r.units[t].main.connect(r.units[a].se)}r.units[t].resume&&r.units[t].resume.bufferOffset?r.units[t].resume.duration.type==="just"?r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.bufferOffset,r.units[t].resume.duration.value):r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.bufferOffset):r.units[t].resume&&r.units[t].resume.loopStart?r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.loopStart):r.units[t].main.start(r.deprecatedWriteHead+e.o)}}}}},qO=function(t){return function(e){return function(r){return function(){if(!!r.units[t].onOff){r.units[t].onOff=!1;var n=r.units[t].main;n.addEventListener("ended",()=>{n.disconnect()}),n.stop(r.deprecatedWriteHead+e.o)}}}}};function Jd(t){for(var e=new Float32Array(t.length),r=0;r<t.length;r++)e[r]=t[r];return e}function ks(t){return function(){t.stop()}}function jd(t){return function(e){return function(r){return function(){var n=[];r.ondataavailable=function(a){n.push(a.data)},r.onstop=function(){var a=new Blob(n,{type:t});e(a)(),n=null}}}}}function Xd(t){return function(e){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:e})}}}function F_(t){return function(){var e=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(e),e}}function Qd(t){return function(){var e=t.createConstantSource();return e.offset.value=0,e.connect(t.destination),e.start(),function(){e.stop(),e.disconnect(t.destination)}}}var Kd=function(t){return function(e){return function(r){return function(){for(var n=new Float32Array(e.length),a=new Float32Array(r.length),u=0;u<e.length;u++)n[u]=e[u];for(var u=0;u<r.length;u++)a[u]=r[u];return t.createPeriodicWave(n,a,{disableNormalization:!0})}}}};function Qi(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},scopes:{},unsu:{},toConnect:{}}}}function Yd(t){return function(){t.close()}}function Zd(t){return function(){return fetch(t).then(function(e){return e.arrayBuffer()},function(e){return console.error("Error fetching buffer",e),Promise.reject(e)})}}function tb(t){return function(e){return function(){return t.decodeAudioData(e)}}}function eb(){return new(window.AudioContext||window.webkitAudioContext)}function rb(t){return function(){return t.state}}function $_(t){return function(){return t.currentTime}}function UE(t){return function(e){return function(r){return function(){t.then(r,e)}}}}var VO=function(t){return function(e){return Pc(function(r){return L_(x)(Pt(IE))(UE(e)(function(n){return r(Yt.create(t(n)))()})(function(n){return r(Zt.create(n))()}))})}};var JO=function(t){return La(function(e){return Lo("Promise failed, couldn't extract JS Error or String")})(Z(et))(tD(O(Zv(Pm)(Qu))(iD(Qu)("Error")(t))(p(o_(Fo))(Lo)(fD(Qu)(t)))))},qE=VO(JO),gs=function(t){return q(Rr)(mr(Ir)(t))(qE)};function nb(t){return function(){return URL.createObjectURL(t)}}var HE=function(t){return function(e){return function(r){return Tt(jd(t))(r)(function(){var n=Yn(hr)(e);return function(a){return n(nb(a))}}())}}};var Wf={ids:p(x)(Xt(B_))(Gu),deleteFromCache:zD,disconnectXFromY:JD,connectXToY:VD,makeAllpass:jD,makeAnalyser:XD,makeAudioWorkletNode:QD,makeBandpass:KD,makeConstant:YD,makeConvolver:ZD,makeDelay:td,makeDynamicsCompressor:ed,makeGain:rd,makeHighpass:nd,makeHighshelf:ad,makeIIRFilter:ud,makeLoopBuf:od,makeLowpass:id,makeLowshelf:fd,makeMediaElement:cd,makeMicrophone:ld,makeNotch:_d,makePeaking:pd,makePeriodicOsc:sd,makePlayBuf:md,makeRecorder:vd,makeSawtoothOsc:Dd,makeSinOsc:dd,makeSpeaker:bd,setDuration:Nd,makeSquareOsc:yd,makeStereoPanner:Ad,makeTriangleOsc:kd,makeWaveShaper:gd,setAnalyserNodeCb:Cd,setMediaRecorderCb:Ed,setWaveShaperCurve:Sd,setAudioWorkletParameter:hd,setBuffer:Fd,setConvolverBuffer:$d,setPeriodicOsc:Md,setOnOff:Vd,setBufferOffset:Rd,setLoopStart:Od,setLoopEnd:Id,setRatio:Gd,setOffset:Wd,setAttack:Bd,setGain:Td,setQ:xd,setPan:wd,setThreshold:Pd,setRelease:Ld,setKnee:Ud,setDelay:qd,setPlaybackRate:Hd,setFrequency:zd},Ct=function(t){return function(e){return q(Rr)(gs(Zd(e)))(function(){var r=tb(t);return function(n){return gs(r(n))}}())}},M_=function(t){var e=mr(t);return function(r){return e(rb(r))}};var oa=function(t){return mr(t)(eb)},zu=function(t){var e=mr(t);return function(r){return e(Qd(r))}},xn=function(t){return function(e){return mr(t)(function(){var n=M_(ir)(e)();return $n(c)(n!=="closed")(Yd(e))()})}},YO=ft,ZO=ft,Cs=function(t){return function(e){return p(di)(function(r){return{microphone:function(){return t?h(xo)(YO(r)):V.value}(),camera:function(){return e?h(xo)(ZO(r)):V.value}()}})(gs(Xd(t)(e)))}};var Vo=function(){function t(){}return t.value=new t,t}(),Jo=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Eu=function(){function t(){}return t.value=new t,t}(),vn=BD,Ai=function(t){return ko(Ln)(O(RE)(Nn(Ln)(q(Rr)(PE(t))(mr(Ir))))(Nn(Ln)(OE(Lo("We navigated away from the page"))(t))))},Nc=function(t){return function(e){return function(r){return function(n){return O(t)(Y(e)(Eu.value))(n)}}}},wa=function(t){return function(e){return function(r){return function(n){return O(t)(Y(e)(K(sr)(pr.value)(Qe(T(n)))))(p(t.Functor0())(function(a){return K(sr)(pr.value)(Qe(T(j(tt)(a)(n))))})(p(t.Functor0())(function(a){return a.value0})(r)))}}}},Es=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return function(m){return p(t)(function(s){return K(sr)(pr.value)(Qe(T(function(){if(s.value0 instanceof Vo)return h(c)(void 0);if(s.value0 instanceof Jo)return j(tt)(j(tt)(s.value0.value0)(n(h(c)(void 0))))(a(Eu.value));if(s.value0 instanceof Eu)return function(){s.value1(),a(Vo.value)();var v=go(q(Rr)(oa(Ir))(function(D){return q(Rr)(zu(Ir)(D))(function(l){return q(Rr)(u(D))(function(C){return mr(Ir)(function(){var vt=f(D)(C)(),Gt=j(tt)(j(tt)(vt)(l))(xn(ir)(D));return a(new Jo(Gt))(),Gt})})})}))();return Gr(e)(hr)(n(function(){return a(Eu.value)(),zo(Ai(v))()}))(function(){return h(c)(void 0)})()};throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 48, column 21 - line 66, column 26): "+[s.value0.constructor.name])}())))})(On(r)(O(r.Plus0().Alt0())(Y(r)(h(c)(void 0)))(p(t)(function(s){return s.value0})(i)))(p(t)(nt.create)(m)))}}}}}}}}},Pa=function(t){return function(e){return function(r){return function(){return t(r)(),e(new NE(r))()}}}},Ss=function(t){return function(e){return function(r){return function(n){return function(a){return Zr(o)(function(u){return function(f){var i=Nc(N(c))(S(o))(e)(f);return fc(o)(O(N(c))(Y(S(o))(K(Ep)(Jt.value)("cursor: pointer;")))(Es(k)(Sr)(S(o))(r)(u)(n)(a)(e)(i)))([ln(ue)(p(k)(function(m){if(m instanceof Eu)return t;if(m instanceof Vo)return"\u23F3";if(m instanceof Jo)return"\u{1F6D1}";throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 115, column 19 - line 118, column 37): "+[m.constructor.name])})(i))])}})}}}}},ht=function(t){return function(e){return function(r){return function(n){return Zr(o)(function(a){return function(u){var f=Nc(N(c))(S(o))(t)(u);return Tn(o)(Es(k)(Sr)(S(o))(e)(a)(r)(n)(t)(f))([ln(ue)(p(k)(function(i){if(i instanceof Eu)return"Turn on";if(i instanceof Vo)return"Loading...";if(i instanceof Jo)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 89, column 19 - line 92, column 44): "+[i.constructor.name])})(f))])}})}}}};var Lc=function(t){return function(e){return function(){var n=Qi(t)(),a=St(If([new rs(p(k)(function(u){return En.create(aC(u))})(e))])(Wf))(function(u){return u(n)})();return a}}};var mt=function(t){return function(e){return function(){var n=Qi(t)(),a=St(If(e)(Wf))(function(u){return u(n)})();return a}}},hs=function(t){return function(){var r=oa(ir)();return p(x)(function(n){return j(tt)(n)(xn(ir)(r))})(mt(r)(t))()}};var tI=function(){return d.value}(),zE=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2 id="allpass">Allpass filter</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">all-pass filter</a> <a href="https://en.wikipedia.org/wiki/All-pass_filter">passes through all frequencies of a source at equal volume but changes their phase</a>. Its use by itself is imperceptible, as the human ear (mostly) does not pick up on phase shifts by themselves. However, when an all-pass filter's output is mixed with several chained all-pass filters plus the original source, you hear a neat phaser effect.</p>

  <p>The <code>bangOn</code> is an event that turns the loop buffer on. We'll learn more about turning things on and off in the "Events" section.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
    \\b _ -> gain_ 0.2
      [ b
      , allpass_ 700.0
          [ allpass_ { frequency: 990.0, q: 20.0 } [ b ]
          , allpass_ 1110.0
              [ b
              , allpass_ { frequency: 2010.0, q: 30.0 } [ b ]
              ]
          ]
      ]
  ]
</code></pre>

  @allpass@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(I()(Q(c))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})(o))(d.value)(tI)({allpass:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([$a(le(ge)(a)(_t()))(function(u){return function(f){return Ot(it)(.2)([u,A_(PD)(700)([A_(_s(yt(At()(z(z(bt)(yD)()()()({reflectSymbol:function(){return"q"}}))(cs)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:990,q:20})([u]),A_(PD)(1110)([u,A_(_s(yt(At()(z(z(bt)(yD)()()()({reflectSymbol:function(){return"q"}}))(cs)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:2010,q:30})([u])])])])}})])}})))})}}};function yi(t){return function(r,n,a){if(n===null)return new t(r);var u=r.byteLength,f=t.BYTES_PER_ELEMENT,i=Math.min(u,n>>>0);if(a===null)return new t(r,i);var m=Math.min((u-i)/f,a);return new t(r,i,m)}}var rI=yi(Uint8ClampedArray),nI=yi(Uint32Array),aI=yi(Uint16Array),VE=yi(Uint8Array),uI=yi(Int32Array),oI=yi(Int16Array),iI=yi(Int8Array),fI=yi(Float32Array),cI=yi(Float64Array);function JE(t){for(var e=t.length,r=new Array(e),n=0;n<e;n++)r[n]=t[n];return r}var Ts={create:VE,BinaryValue0:function(){}};var xs=function(t){return function(e){return function(){return JE(e)}}};var Wc=bu,Gc=bu,Bc=bu,Ya=bu,Za=bu,tu=bu,eu=bu,ru=bu;function Fs(t){return t|0}var ki=function(){return window};function KE(t,e,r,n){if(typeof window<"u"){var a=window[r];if(a!=null&&n instanceof a)return e(n)}for(var u=n;u!=null;){var f=Object.getPrototypeOf(u),i=f.constructor.name;if(i===r)return e(n);if(i==="Object")return t;u=f}return t}var Nt=function(t){return function(e){return KE(V.value,B.create,t,e)}};var ab=Nt("HTMLCanvasElement");function YE(t){return function(){return t.body}}var ZE=function(){var t=p(x)(tn);return function(e){return t(YE(e))}}();var tS=ft;function Gf(t){return function(){return t.valueAsNumber}}var Uc=Nt("HTMLInputElement");function ob(t){return function(){return t.document}}function $s(t){return function(e){return function(){return e.requestAnimationFrame(t)}}}var ib=ft;var QI=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}},Ki=qt(function(t){return function(){var r=ki(),n=_r(!0)(),a=QI("fx","FRP.Event.Animate",function(){return Kt(x)(Tt($s)(r)(function(){var i=wr(n)();return $n(c)(i)(function(){return t(void 0)(),a(19)()})()}))}),u=a(15);return u(),Mn(!1)(n)}});var KI="background-color: rgb(150,30,10);",YI="background-color: rgb(130,60,10);",ZI="background-color: rgb(80,90,10);",tR="background-color: rgb(10,130,10);",eR="background-color: rgb(10,100,0);",rR=Vp(Xa)(function(t){return we(Me(Hp)()(Ha)()(a_))(KI)(we(Me(ua)()(An)()(Ha))(YI)(we(Me(Au)()(yn)()(An))(ZI)(we(Me(yu)()(kn)()(yn))(tR)(we(Me(ku)()(gu)()(kn))(eR)(Uu)))))}),nR=function(t){return function(e){return function(r){return function(n){return Ds(vs(yt(At()(z(z(bt)(ms)()()()({reflectSymbol:function(){return"fftSize"}}))(t)()()()({reflectSymbol:function(){return"cb"}})))(dt()())))({cb:n,fftSize:ts.value})([le(e)(r)(_t())])}}}},aR=function(){return d.value}(),Ie="background-color: rgb(255,255,255,0.0);",Ne=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return function(m){return function(s){return function(_){return p(t)(function(v){var D=u_(e)()(u_(n)()(v)(m))(s);return D?K(u)(Jt.value)(u_(e)()(u_(n)()(rR)(m))(s)):K(u)(Jt.value)(Ie)})(_)}}}}}}}}}}},uR=function(){return 15/40}(),oR=function(){return 10/40}(),iR=function(){return 7/40}(),fR=function(){return 3/40}(),cR=function(){return 1/40}(),rS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Wags provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Wags as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})(o))(o)(aR)({analyser:R(rt(Zr(o)(function(n){return function(a){var u=Tl(mu(c))(Z(et))(a),f=Nc(N(c))(S(o))(r)(function(m){return m.right}(u)),i=function(m){return m.left}(u);return Be(o)([Tn(o)(O(N(c))(Y(S(o))(K(oc)(Jt.value)("cursor: pointer;")))(Es(k)(Sr)(S(o))(t)(function(m){return n(Zt.create(m))})(function(m){return Ct(m)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(m){return function(s){return function(){var v=_r(V.value)(),D=Qi(m)(),l=If([nR(ss)(ge)(s)(function(ot){return function(){return Mn(new B(ot))(v)(),Mn(V.value)(v)}})])(Wf),C=St(O(N(c))(p(k)(Zt.create)(l))(p(k)(Yt.create)(Ki)))(function(ot){if(ot instanceof Zt)return ot.value0(D);if(ot instanceof Yt)return function(){var Gt=wr(v)();return ea(c)(Je)(Gt)(function(re){return function(){var gr=F_(re)(),lt=xs(Ts)(gr)(),oe=_r(0)(),ke=_r(0)(),Bt=_r(0)(),vr=_r(0)(),Su=_r(0)(),Vu=_r(0)(),xi=_r(0)(),au=_r(0)(),Dn=_r(0)(),Xo=_r(0)(),Uf=function(Fr){if(Fr<32)return oe;if(Fr<64)return ke;if(Fr<96)return Bt;if(Fr<128)return vr;if(Fr<168)return Su;if(Fr<160)return Vu;if(Fr<224)return xi;if(Ze)return au;throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 143, column 45 - line 151, column 63): "+[Fr.constructor.name])};ll(lt)(function(Fr){var Qo=Fs(Fr);return function(){var Ko=wr(Xo)();return pf(Le(xu)(Qo))(Dn)(),pf(Le(xu)(Qo))(Uf(Ko))(),pf(Le(xu)(1))(Xo)()}})();var qf=ra(dg)(c)(function(Fr){return function(){var Hf=p(x)(qe)(wr(Fr))(),Ko=p(x)(Ku(cl)(Hf))(p(x)(qe)(wr(Dn)))();return we(Me(Hp)()(Ha)()(a_))(Ko>uR)(we(Me(ua)()(An)()(Ha))(Ko>oR)(we(Me(Au)()(yn)()(An))(Ko>iR)(we(Me(yu)()(kn)()(yn))(Ko>fR)(we(Me(ku)()(gu)()(kn))(Ko>cR)(Uu)))))}})(we(Me(sg)()(Jv)()(Dg))(oe)(we(Me(mg)()(jv)()(Jv))(ke)(we(Me(vg)()(a_)()(jv))(Bt)(we(Me(Hp)()(Ha)()(a_))(vr)(we(Me(ua)()(An)()(Ha))(Su)(we(Me(Au)()(yn)()(An))(Vu)(we(Me(yu)()(kn)()(yn))(xi)(we(Me(ku)()(gu)()(kn))(au)(Uu)))))))))();return n(new Yt(qf))()}})()};throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 124, column 57 - line 162, column 57): "+[ot.constructor.name])})();return function(){return C(),function(){var Gt=M_(ir)(m)();return $n(c)(Gt!=="closed")(xn(ir)(m))()}(),n(new Yt(Vp(Xa)(T(Vp(Fa)(T(!1))))))()}}}})(r)(f)))([ln(ue)(p(k)(function(m){if(m instanceof Eu)return"Turn on";if(m instanceof Vo)return"Loading...";if(m instanceof Jo)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 175, column 31 - line 178, column 56): "+[m.constructor.name])})(f))]),Te(o)(Y(S(o))(K(pt)(Jt.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")))([Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(xr)(da)(xr)(Ao)(pt)(da)(Ao)(ru)(ru)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(xr)(da)(jn)(bo)(pt)(da)(bo)(eu)(ru)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(xr)(da)(Jn)(Do)(pt)(da)(Do)(tu)(ru)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(xr)(da)(Vn)(vo)(pt)(da)(vo)(Za)(ru)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(xr)(da)(zn)(mo)(pt)(da)(mo)(Ya)(ru)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(xr)(da)(Fa)(so)(pt)(da)(so)(Bc)(ru)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(xr)(da)(Bo)(po)(pt)(da)(po)(Gc)(ru)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(xr)(da)(Go)(_o)(pt)(da)(_o)(Wc)(ru)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(jn)(Da)(xr)(Ao)(pt)(Da)(Ao)(ru)(eu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(jn)(Da)(jn)(bo)(pt)(Da)(bo)(eu)(eu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(jn)(Da)(Jn)(Do)(pt)(Da)(Do)(tu)(eu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(jn)(Da)(Vn)(vo)(pt)(Da)(vo)(Za)(eu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(jn)(Da)(zn)(mo)(pt)(Da)(mo)(Ya)(eu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(jn)(Da)(Fa)(so)(pt)(Da)(so)(Bc)(eu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(jn)(Da)(Bo)(po)(pt)(Da)(po)(Gc)(eu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(jn)(Da)(Go)(_o)(pt)(Da)(_o)(Wc)(eu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Jn)(va)(xr)(Ao)(pt)(va)(Ao)(ru)(tu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Jn)(va)(jn)(bo)(pt)(va)(bo)(eu)(tu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Jn)(va)(Jn)(Do)(pt)(va)(Do)(tu)(tu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Jn)(va)(Vn)(vo)(pt)(va)(vo)(Za)(tu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Jn)(va)(zn)(mo)(pt)(va)(mo)(Ya)(tu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Jn)(va)(Fa)(so)(pt)(va)(so)(Bc)(tu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Jn)(va)(Bo)(po)(pt)(va)(po)(Gc)(tu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Jn)(va)(Go)(_o)(pt)(va)(_o)(Wc)(tu)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Vn)(ma)(xr)(Ao)(pt)(ma)(Ao)(ru)(Za)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Vn)(ma)(jn)(bo)(pt)(ma)(bo)(eu)(Za)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Vn)(ma)(Jn)(Do)(pt)(ma)(Do)(tu)(Za)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Vn)(ma)(Vn)(vo)(pt)(ma)(vo)(Za)(Za)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Vn)(ma)(zn)(mo)(pt)(ma)(mo)(Ya)(Za)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Vn)(ma)(Fa)(so)(pt)(ma)(so)(Bc)(Za)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Vn)(ma)(Bo)(po)(pt)(ma)(po)(Gc)(Za)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(Vn)(ma)(Go)(_o)(pt)(ma)(_o)(Wc)(Za)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(zn)(sa)(xr)(Ao)(pt)(sa)(Ao)(ru)(Ya)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(zn)(sa)(jn)(bo)(pt)(sa)(bo)(eu)(Ya)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(zn)(sa)(Jn)(Do)(pt)(sa)(Do)(tu)(Ya)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(zn)(sa)(Vn)(vo)(pt)(sa)(vo)(Za)(Ya)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(zn)(sa)(zn)(mo)(pt)(sa)(mo)(Ya)(Ya)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(zn)(sa)(Fa)(so)(pt)(sa)(so)(Bc)(Ya)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(zn)(sa)(Bo)(po)(pt)(sa)(po)(Gc)(Ya)(i)))([]),Te(o)(O(N(c))(Y(S(o))(K(pt)(Jt.value)(Ie)))(Ne(k)(zn)(sa)(Go)(_o)(pt)(sa)(_o)(Wc)(Ya)(i)))([])])])}})))})}}};var _R=function(){return d.value}(),nS=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2 id="bandpass">Bandpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">bandpass filter</a> isolates a single frequency range of a source. When you crank up a bandpass node's Q value, the isolation gets more intense. At the extreme, the source signal is almost lost and you get a pure sound that resembles a sine-wave oscillator.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
    \\b _ -> gain_ 0.8
      [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
      , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
      , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
      , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
      , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
      ]
  ]</code></pre>

  @bandpass@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(I()(Q(c))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})(o))(d.value)(_R)({bandpass:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([$a(le(ge)(a)(_t()))(function(u){return function(f){return Ot(it)(.8)([hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:400,q:1})([u]),hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:880,q:5})([u]),hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:1200,q:10})([u]),hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:2e3,q:20})([u]),hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:3e3,q:30})([u])])}})])}})))})}}};var sR=function(){return d.value}(),aS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="compression">Compression</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode">Compression</a>, when used judiciously, can make certain sounds sit better in a mix, like for example vocals. The <a href="https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode">MDN Web Audio documentation</a> does an excellent job explaining how its parameters work. When used not-judiciously, it makes everything sound loud, and who likes that? So let's use it judiciously, like in the example below. We'll pass an object that only specifies the threshold and otherwise use the default options for the compressor.</p>

  <pre><code>
-- defaultDynamicsCompressor =
--   { ratio: 12.0
--   , attack: 0.003
--   , release: 0.25
--   , knee: 30.0
--   , threshold: -24.0
--   }
run2_
  [ dynamicsCompressor_ { threshold: -50.0 }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~compression~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})(o))(o)(sR)({compression:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([lE(XC(yt(At()(bt))(dt()())))({})([le(ge)(a)(_t())])])}})))})}}};var ia=function(){return function(t){var e=fn(),r=Ke()({reflectSymbol:function(){return"playbackRate"}})(d.value),n=wf(t);return function(a){return e(r(n(a)))}}},Bf=function(){return function(t){var e=fn(),r=Ke()({reflectSymbol:function(){return"onOff"}})(d.value),n=uC(t);return function(a){return e(r(n(a)))}}},uS=function(){return function(t){var e=fn(),r=Ke()({reflectSymbol:function(){return"offset"}})(d.value),n=wf(t);return function(a){return e(r(n(a)))}}},oS=function(){var t=fn(),e=Ke()({reflectSymbol:function(){return"loopStart"}})(d.value);return function(r){return t(e(r))}},iS=function(){var t=fn(),e=Ke()({reflectSymbol:function(){return"loopEnd"}})(d.value);return function(r){return t(e(r))}},Fn=function(){return function(t){var e=fn(),r=Ke()({reflectSymbol:function(){return"gain"}})(d.value),n=wf(t);return function(a){return e(r(n(a)))}}},Co=function(){return function(t){var e=fn(),r=Ke()({reflectSymbol:function(){return"frequency"}})(d.value),n=wf(t);return function(a){return e(r(n(a)))}}};var qc=function(){return function(t){var e=fn(),r=Ke()({reflectSymbol:function(){return"delayTime"}})(d.value),n=wf(t);return function(a){return e(r(n(a)))}}};var vR=function(){return d.value}(),fS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Wags uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>bang</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(I()(I()(I()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}})(o))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})(o))(o)(vR)({tf:R(or(ue)("<|>")),txt:R(or(ue)(`run2_
  [ gain_ 0.5
      [ constant 0.0
          ( bangOn <|>
              ( bang $ offset $ AudioEnvelope
                  { d: 5.0
                  , o: 0.1
                  , p: 0 .. 1920 # mapWithIndex
                      \\i -> const $
                      if i \`mod\` 3 == 0 then 1.0
                      else 0.0
                  }
              )
          )
      ]
  ]`)),constant:R(rt(ht(r)(t)(function(n){return h(ba)(void 0)})(function(n){return function(a){return mt(n)([Ot(it)(.5)([bs(fs)(0)(O(N(c))(_t())(Y(S(o))(uS()(Rn)({d:5,o:.1,p:ro(ni)(function(u){return T(function(){var f=iu($o)(u)(3)===0;return f?1:0}())})(dn(0)(1920))}))))])])}})))})}}};var dR=function(){return d.value}(),cS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})(o))(o)(dR)({convolution:R(rt(ht(r)(t)(function(n){return zt(ME)(p(di)(function(a){return function(u){return{loop:a,verb:u}}})(Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(Ct(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(a){return mt(n)([uE(bC)(a.verb)([le(ge)(a.loop)(_t())])])}})))})}}};var AR=function(){return d.value}(),lS=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2 id="delay">Delay</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/DelayNode">Delay</a>, as its name suggests, delays a signal. Using multiple delay nodes, you can create a decent echo effect.</p>

  <p>To create an even <i>better</i> echo effect, you can used fixed points, which is covered in the <a>Fix and fan</a> section of this documentation.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (playBuf buf bangOn)
      \\b _ -> gain_ 0.2
        [ delay_ 0.03 [ b ]
        , delay_ 0.1 [ b ]
        , delay_ 0.3 [ b ]
        , delay_ 0.7 [ b ]
        ]
  ]</code></pre>

  @delay@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(I()(Q(c))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})(o))(d.value)(AR)({delay:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return mt(n)([$a(Qn(za)(a)(_t()))(function(u){return function(f){return Ot(it)(.2)([yo(en)(.03)([u]),yo(en)(.1)([u]),yo(en)(.3)([u]),yo(en)(.7)([u])])}})])}})))})}}};var kR=function(){return d.value}(),_S=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="gain">Gain</h2>
  <p>The almighty <a href="https://developer.mozilla.org/en-US/docs/Web/API/GainNode">gain</a> node is your friendly neighborhood volume control. Volume in the web-audio API goes from 0 to 1 whereas we hear logarithmically, so when you're using this, make sure to convert between decibels and gain if you want to work with more intuitive units. The conversion formula is as follows:</p>

  <pre><code>decibels = 20 * log10( gain );</code></pre>

  <p>And here's a little example of a single gain node:</p>

  <pre><code>run2_
  [ gain_ 0.1
    [ loopBuf buf bangOn ]
  ] </code></pre>

  ~gain~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})(o))(o)(kR)({gain:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return mt(n)([Ot(it)(.1)([le(ge)(a)(_t())])])}})))})}}};var CR=function(){return d.value}(),pS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})(o))(o)(CR)({highpass:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Fc(Qa)(2e3)([le(ge)(a)(_t())])])}})))})}}};var SR=function(){return d.value}(),sS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})(o))(o)(SR)({highshelf:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([_E(jC(yt(At()(z(z(bt)(SC)()()()({reflectSymbol:function(){return"gain"}}))(hC)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:2e3,gain:.4})([le(ge)(a)(_t())])])}})))})}}};var TR=function(){return d.value}(),mS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="iir">IIR filter</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/IIRFilterNode">IIR filter</a>, or infinite impulse response filter, is the Swiss Army Knife of filters. You can carve out and boost parts of the spectrum with amazing precision. But it comes with a catch: you can't automate the parameters. The parameters are also tough to work with if you're new to IIR filters. In short, you're setting up coefficients for a filter of type:</p>

  <pre><code>x0s0 + x1s1 + x2s2 + ... + y0S0 + y1S1 + y2S2 + ...</code></pre>

  <p>Where <code>s1</code> is the unfiltered signal at time <code>t-1</code>, <code>S0</code> is the <i>filtered</i> signal at time <code>t-1</code>, etc. The xs and ys are often called <i>feedforward</i> and <i>feedback</i> coefficients respectively.</p>

  <p>Because the Web Audio API accepts between 3 and 20 parameters for feedforward and feedback coefficients, Wags enforces that through a <a href="https://github.com/bodil/purescript-sized-vectors">sized vector</a>.</p>

  <pre><code>\\{loop, verb} -> run2_
  [ iirFilter
      ( (0.00020298 : 0.0004059599 : 0.00020298 : empty)
          /\\ (1.0126964558 : -1.9991880801 : 0.9873035442 : empty)
      )
      [ loopBuf buf bangOn ]
  ]</code></pre>
  ~iirFilterEx~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}})(o))(o)(TR)({iirFilterEx:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([oE()()(dC(Wu)(Wu))(new nt(bf()()(20298e-8)(bf()()(.0004059599)(bf()()(20298e-8)(qm))),bf()()(1.0126964558)(bf()()(-1.9991880801)(bf()()(.9873035442)(qm)))))([le(ge)(a)(_t())])])}})))})}}};var FR=function(){return d.value}(),vS=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2 id="loopbuf">Looping buffer</h2>

  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode">looping buffer</a> is buffered audio that loops. The buffered audio is usually a sound file, but it'll play anything you write to a buffer. Like in the Web Audio API, you can set the buffer's start and end and optionally its duration.</p>

  <pre><code>\\buf -> run2_
  [ loopBuf
      { buffer: buf
      , playbackRate: 0.5
      , loopStart: 0.1
      , loopEnd: 0.6
      }
      bangOn
  , loopBuf
      { buffer: buf
      , playbackRate: 1.0
      , loopStart: 0.5
      , loopEnd: 1.2
      }
      bangOn
  , loopBuf
      { buffer: buf
      , playbackRate: 1.7
      }
      bangOn
  ]</code></pre>

  @loopBuf@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(I()(Q(c))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})(o))(d.value)(FR)({loopBuf:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(a){return mt(n)([le(Of(yt(At()(z(z(z(z(bt)(hc)()()()({reflectSymbol:function(){return"playbackRate"}}))(m_)()()()({reflectSymbol:function(){return"loopStart"}}))(s_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Pf)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())))({buffer:a,playbackRate:.5,loopStart:.1,loopEnd:.6})(_t()),le(Of(yt(At()(z(z(z(z(bt)(hc)()()()({reflectSymbol:function(){return"playbackRate"}}))(m_)()()()({reflectSymbol:function(){return"loopStart"}}))(s_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Pf)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())))({buffer:a,playbackRate:1,loopStart:.5,loopEnd:1.2})(_t()),le(Of(yt(At()(z(z(bt)(hc)()()()({reflectSymbol:function(){return"playbackRate"}}))(Pf)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())))({buffer:a,playbackRate:1.7})(_t())])}})))})}}};var MR=function(){return d.value}(),DS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})(o))(o)(MR)({lowpass:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([$c(ls)(215)([le(ge)(a)(_t())])])}})))})}}};var PR=function(){return d.value}(),dS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})(o))(o)(PR)({lowshelf:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([sE(JC(yt(At()(z(z(bt)(gC)()()()({reflectSymbol:function(){return"gain"}}))(CC)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:91,gain:.4})([le(ge)(a)(_t())])])}})))})}}};var IR=function(){return d.value}(),bS=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2 id="microphone">Microphone</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioSourceNode">microphone</a> will use your microphone if you give the browser permission to do so.</p>

  <blockquote>Make sure to use \u{1F3A7} when you run this example! Otherwise, you'll cause quite a stir in whatever internet cafe, household or public restroom you're perusing this documentation in.</blockquote>

  <pre><code>\\mic -> run2_
  [ case mic of
      Just m -> fix \\i -> gain_ 1.0
        [ microphone m
        , delay_ 0.1 [ gain_ 0.2 [ input i ] ]
        ]
      Nothing -> gain_ 0.02 [ sinOsc_ 440.0 ]
  ]</code></pre>

  @microphone@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(I()(Q(c))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})(o))(d.value)(IR)({microphone:R(rt(ht(r)(t)(function(n){return Cs(!0)(!1)})(function(n){return function(a){return mt(n)([function(){if(a.microphone instanceof B)return Ka(function(u){return Ot(it)(1)([d_(__)(a.microphone.value0),yo(en)(.1)([Ot(it)(.2)([u])])])});if(a.microphone instanceof V)return Ot(it)(.02)([fE(Xi)(440)]);throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Microphone (line 43, column 15 - line 48, column 56): "+[a.microphone.constructor.name])}()])}})))})}}};var NR=function(){return d.value}(),AS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
<h2 id="notch">Notch filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">notch filter</a>, also known as a band-reject filter, attenuates a single frequency range of a source. When you crank up their Q value, the attenuation gets more intense. At the extreme, it sounds like part of the source got sucked into a vacuum, which is not un-interesting!</p>

  <pre><code>\\buf -> run2_
  [
    notch_ { frequency: 400.0, q: 1.0 }
    $ pure $ notch_ { frequency: 880.0, q: 5.0 }
    $ pure $ notch_ { frequency: 1200.0, q: 10.0 }
    $ pure $ notch_ { frequency: 2000.0, q: 20.0 }
    $ pure $ notch_ { frequency: 3000.0, q: 30.0 }
    $ pure $ loopBuf buf bangOn
  ]</code></pre>

  ~notch~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})(o))(o)(NR)({notch:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Mc(xc(yt(At()(z(z(bt)(Ec)()()()({reflectSymbol:function(){return"q"}}))(Sc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:400,q:1})(h(lr)(Mc(xc(yt(At()(z(z(bt)(Ec)()()()({reflectSymbol:function(){return"q"}}))(Sc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:880,q:5})(h(lr)(Mc(xc(yt(At()(z(z(bt)(Ec)()()()({reflectSymbol:function(){return"q"}}))(Sc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:1200,q:10})(h(lr)(Mc(xc(yt(At()(z(z(bt)(Ec)()()()({reflectSymbol:function(){return"q"}}))(Sc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:2e3,q:20})(h(lr)(Mc(xc(yt(At()(z(z(bt)(Ec)()()()({reflectSymbol:function(){return"q"}}))(Sc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:3e3,q:30})(h(lr)(le(ge)(a)(_t())))))))))))])}})))})}}};var WR=function(){return d.value}(),yS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="peaking">Peaking filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">peaking filter</a> is sort of like a notch/bandpass combo. It sounds different than bandpass or notch, and is often a better choice depending on what you're making. The Q works as normal, but the gain either boosts or attenuates the frequency in question if it is positive or negative.</p>

  <pre><code>\\buf -> run2_
  [
    peaking_ { frequency: 400.0, q: 1.0, gain: -20.0 }
    $ pure $ peaking_ { frequency: 880.0, q: 5.0, gain: 20.0 }
    $ pure $ peaking_ { frequency: 1200.0, q: 10.0, gain: -20.0 }
    $ pure $ peaking_ { frequency: 2000.0, q: 20.0, gain: 20.0 }
    $ pure $ peaking_ { frequency: 3000.0, q: 30.0, gain: -20.0 }
    $ pure $ loopBuf buf bangOn
  ]</code></pre>

  ~peaking~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})(o))(o)(WR)({peaking:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([wc(Tc(yt(At()(z(z(z(bt)(kc)()()()({reflectSymbol:function(){return"q"}}))(gc)()()()({reflectSymbol:function(){return"gain"}}))(Cc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:400,q:1,gain:-20})(h(lr)(wc(Tc(yt(At()(z(z(z(bt)(kc)()()()({reflectSymbol:function(){return"q"}}))(gc)()()()({reflectSymbol:function(){return"gain"}}))(Cc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:880,q:5,gain:20})(h(lr)(wc(Tc(yt(At()(z(z(z(bt)(kc)()()()({reflectSymbol:function(){return"q"}}))(gc)()()()({reflectSymbol:function(){return"gain"}}))(Cc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:1200,q:10,gain:-20})(h(lr)(wc(Tc(yt(At()(z(z(z(bt)(kc)()()()({reflectSymbol:function(){return"q"}}))(gc)()()()({reflectSymbol:function(){return"gain"}}))(Cc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:2e3,q:20,gain:20})(h(lr)(wc(Tc(yt(At()(z(z(z(bt)(kc)()()()({reflectSymbol:function(){return"q"}}))(gc)()()()({reflectSymbol:function(){return"gain"}}))(Cc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:3e3,q:30,gain:-20})(h(lr)(le(ge)(a)(_t())))))))))))])}})))})}}};var BR=function(){return d.value}(),kS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="periodic">Periodic wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">periodic wave oscillator</a> plays back a custom periodic waveform at a given frequency. The custom waveform must be set as part of the initialization and can be changed after initialization. Note that the change will not go into effect if the oscillator is on: it must be turned off and on again.</p>

  <pre><code>\\buf -> run2_
  [
    gain_ 0.2
      [
        periodicOsc
          { frequency: 140.0
          , spec:
              ( (0.1 +> 0.2 +> 0.3 +> 0.4 +> empty)
                  /\\ (0.4 +> 0.3 +> 0.2 +> 0.1 +> empty)
              )
          }
          bangOn
      ]
  ]</code></pre>

  ~periodic~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(BR)({periodic:R(rt(ht(r)(t)(function(n){return h(ba)(void 0)})(function(n){return function(a){return mt(n)([Ot(it)(.2)([Di(vi(yt(At()(z(z(bt)(mi(pi(ua)))()()()({reflectSymbol:function(){return"spec"}}))(si)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:140,spec:new nt(we(Me(ua)()(An)()(Ha))(.1)(we(Me(Au)()(yn)()(An))(.2)(we(Me(yu)()(kn)()(yn))(.3)(we(Me(ku)()(gu)()(kn))(.4)(Uu)))),we(Me(ua)()(An)()(Ha))(.4)(we(Me(Au)()(yn)()(An))(.3)(we(Me(yu)()(kn)()(yn))(.2)(we(Me(ku)()(gu)()(kn))(.1)(Uu)))))})(_t())])])}})))})}}};var qR=function(){return d.value}(),gS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="playbuf">Playing a buffer</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode">Playback from a buffer</a> is one of the bread-and-butter operations in Web Audio (or any audio). The buffered audio is usually a sound file, but it'll play anything you write to a buffer. Like in the Web Audio API, you can set the buffer's start time and optionally its duration.</p>

  <pre><code>\\buf -> run2_
  [
    playBuf
      { buffer
      , duration: 3.0
      , bufferOffset: 4.2
      }
      bangOn
  ]
</code></pre>

  ~playBuf~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})(o))(o)(qR)({playBuf:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(a){return mt(n)([Qn(v_(yt(At()(z(z(z(bt)(yC)()()()({reflectSymbol:function(){return"duration"}}))(AC)()()()({reflectSymbol:function(){return"bufferOffset"}}))(p_)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())))({buffer:a,duration:3,bufferOffset:4.2})(_t())])}})))})}}};var fb=function(){function t(){}return t.value=new t,t}();var CS={attr:function(t){return function(e){return b({key:"controls",value:L(e)})}}};var cb=function(){function t(){}return t.value=new t,t}();var ES={attr:function(t){return function(e){return b({key:"src",value:L(e)})}}};var lb=function(t){return function(e){return function(r){return new U(J(t)("audio")(e)(new H(r)))}}};var jR=function(t){return function(e){return function(r){return function(n){return LD(t)(n)(d_(e)(r))}}}},XR=function(){return d.value}(),SS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})(o))(o)(XR)({recorder:R(rt(Zr(o)(function(n){return function(a){var u=Tl(mu(c))(Z(et))(a),f=Tl(mu(c))(Z(et))(function(_){return _.left}(u)),i=function(_){return _.right}(f),m=Nc(N(c))(S(o))(r)(function(_){return _.right}(u)),s=function(_){return _.left}(f);return Be(o)([Tn(o)(O(N(c))(Y(S(o))(K(oc)(Jt.value)("cursor: pointer;")))(p(k)(function(_){return K(sr)(pr.value)(Qe(T(function(){if(_.e instanceof Vo)return h(c)(void 0);if(_.e instanceof Jo)return j(tt)(j(tt)(j(tt)(_.e.value0)(t(h(c)(void 0))))(ea(c)(Je)(_.rec)(function(){var v=Jl(Mv);return function(D){return v(ks(D))}}())))(n(Zt.create(Eu.value)));if(_.e instanceof Eu)return function(){_.cncl();var D=D_();n(new Zt(Vo.value))();var l=go(q(Rr)(p(di)(function(C){return C.microphone})(Cs(!0)(!1)))(function(C){return mr(Ir)(function(){var vt=Ur(h(c)(h(c)(void 0)))(function(Gt){return function(){var he=oa(ir)(),gr=Qi(he)(),lt=If([jR(DD)(__)(Gt)(function(ke){return function(){return n(new Yt(new Zt(ke)))(),Kt(x)(ps(ke)(D))(),HE("audio/ogg; codecs=opus")(function(vr){return n(Yt.create(Yt.create(vr)))})(ke)()}})])(Wf),oe=St(lt)(function(ke){return ke(gr)})();return function(){oe(),q(hr)(tE(D))(nr(c)(Je)(function(){var vr=Jl(Mv);return function(Su){return vr(ks(Su))}}()))();var Bt=M_(ir)(he)();return $n(c)(Bt!=="closed")(xn(ir)(he))()}}})(C)();return n(new Zt(new Jo(vt)))(),vt})}))();return t(function(){return n(Zt.create(Eu.value))(),zo(Ai(l))()})(),void 0};throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Recorder (line 63, column 47 - line 106, column 52): "+[_.e.constructor.name])}())))})(On(S(o))(O(N(c))(Y(S(o))(V.value))(p(k)(B.create)(i)))(p(k)(tl)(On(S(o))(O(N(c))(Y(S(o))(h(c)(void 0)))(p(k)(function(_){return _.value0})(r)))(p(k)(function(_){return function(v){return function(D){return{e:_,cncl:v,rec:D}}}})(m)))))))([ln(ue)(p(k)(function(_){if(_ instanceof Eu)return"Turn on";if(_ instanceof Vo)return"Loading...";if(_ instanceof Jo)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Recorder (line 117, column 31 - line 120, column 56): "+[_.constructor.name])})(m))]),Be(o)([lb(o)(O(N(c))(Y(S(o))(K(CS)(fb.value)("true")))(O(N(c))(Y(S(o))(K(sv)(Jt.value)("display:none;")))(O(N(c))(p(k)(function(_){return K(ES)(cb.value)(_)})(s))(p(k)(T(K(sv)(Jt.value)("display:block;")))(s)))))([])])])}})))})}}};var KR=function(){return d.value}(),hS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(KR)({periodic:R(rt(ht(r)(t)(function(n){return h(ba)(void 0)})(function(n){return function(a){return mt(n)([Ot(it)(.2)([iE(DC)(448)(_t())])])}})))})}}};var ZR=function(){return d.value}(),TS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(ZR)({periodic:R(rt(ht(r)(t)(function(n){return h(ba)(void 0)})(function(n){return function(a){return mt(n)([Ot(it)(.2)([Rf(Xi)(448)(_t())])])}})))})}}};var eN=function(){return d.value}(),xS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(eN)({periodic:R(rt(ht(r)(t)(function(n){return h(ba)(void 0)})(function(n){return function(a){return mt(n)([Ot(it)(.2)([b_(yc)(448)(_t())])])}})))})}}};var nN=function(){return d.value}(),FS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})(o))(o)(nN)({pan:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return mt(n)([mE(vC)(1)([le(ge)(a)(_t())])])}})))})}}};var uN=function(){return d.value}(),$S=$t({reflectType:function(){return`<ul>
  <li><a href="#allpass">All-pass filter</a></li>
  <li><a href="#analyser">Analyser</a></li>
  <li><a href="#bandpass">Bandpass filter</a></li>
  <li><a href="#constant">Constant value</a></li>
  <li><a href="#compression">Compression</a></li>
  <li><a href="#convolution">Convolution</a></li>
  <li><a href="#delay">Delay</a></li>
  <li><a href="#gain">Gain</a></li>
  <li><a href="#highpass">Highpass filter</a></li>
  <li><a href="#highshelf">Highshelf filter</a></li>
  <li><a href="#iir">IIR filter</a></li>
  <li><a href="#loopbuf">Looping buffer</a></li>
  <li><a href="#lowpass">Lowpass filter</a></li>
  <li><a href="#lowshelf">Lowshelf filter</a></li>
  <!--<li><a href="#media">Media element</a></li>-->
  <li><a href="#microphone">Microphone</a></li>
  <li><a href="#notch">Notch filter</a></li>
  <!--<li><a href="#panner">Panner</a></li>-->
  <li><a href="#peaking">Peaking filter</a></li>
  <li><a href="#periodic">Periodic wave oscillator</a></li>
  <li><a href="#playbuf">Playing a buffer</a></li>
  <li><a href="#recorder">Recorder</a></li>
  <li><a href="#sawtooth">Sawtooth wave oscillator</a></li>
  <li><a href="#sine">Sine wave oscillator</a></li>
  <li><a href="#square">Square wave oscillator</a></li>
  <li><a href="#pan">Stereo panner</a></li>
  <li><a href="#triangle">Triangle wave oscillator</a></li>
  <li><a href="#waveshaper">Waveshaper</a></li>
    </ul>
`}})()()(Q(c))(o)(uN)({});var iN=function(){return d.value}(),MS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(iN)({periodic:R(rt(ht(r)(t)(function(n){return h(ba)(void 0)})(function(n){return function(a){return mt(n)([Ot(it)(.2)([ds(is)(448)(_t())])])}})))})}}};var cN=function(){return d.value}(),wS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(I()(I()(Q(c))({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}})(o))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})(o))(o)(cN)({code:R(or(ue)(`do
  let
    makeDistortionCurve :: Number -> Array Number
    makeDistortionCurve k =
      map
        ( \\i ->
            let
              x = (toNumber i * 2.0 / toNumber n_samples) - 1.0
            in
              (3.0 + k) * x * 20.0 * deg / (pi + (k * abs x))
        )
        (0 .. (n_samples - 1))
      where
      n_samples = 44100

      deg = pi / 180.0
  run2_
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]`)),waveShaper:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){var u=function(f){var i=Ji/180;return p(xe)(function(m){var s=qe(m)*2/qe(44100)-1;return(3+f)*s*20*i/(Ji+f*rm(Na)(of)(s))})(dn(0)(44099))};return mt(n)([vE(VC)(Jd(u(400)))([le(ge)(a)(_t())])])}})))})}}};var _N=function(){return d.value}(),PS=function(t){return function(e){return function(r){return function(n){var a=j(tt)(e(Lf.value))(vn),u=Pa(t)(r);return $t({reflectType:function(){return`<div>
  <h1>Audio Units</h1>

  <h3>There sure are a lot of them!</h3>
  <p>
    This section provides a tour of the web audio nodes provided by the Web Audio API and, by extension, Wags. There are only two omissions:</p>
    <ul>
      <li>Audio Worklet Nodes</li>
      <li>Multi-channel audio</li>
    </ul>
    <p>Both of these will be covered in later sections.</p>

  <p>
    This section is long and should be read like those passages in the Bible that list who was the child of who: DILIGENTLY AND COPIOUSLY. That said, if you want to skip around, here's a table of contents.
  </p>
  ~toc~
  <p>And now, without further ado... (~drumroll~) Here are some audio nodes!</p>

  ~allpass~
  ~analyser~
  ~bandpass~
  ~constant~
  ~compression~
  ~convolution~
  ~delay~
  ~gain~
  ~highpass~
  ~highshelf~
  ~iirFilter~
  ~loopBuf~
  ~lowpass~
  ~lowshelf~
  ~microphone~
  ~notch~
  ~peaking~
  ~periodicOsc~
  ~playBuf~
  ~recorder~
  ~sawtoothOsc~
  ~sinOsc~
  ~squareOsc~
  ~pan~
  ~triangleOsc~
  ~waveShaper~

  <h2>Next steps</h2>
  <p>Phew, that was a lot of audio units! In the next section, we'll make them come alive thanks to the magic of <a ~next~ style="cursor:pointer;">events</a>.</p>
</div>`}})()()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(_n()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(Q(c))({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}})(o))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}})(o))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}})(o))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}})(o))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}})(o))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}})(o))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})(o))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})(o))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}})(o))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})(o))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})(o))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})(o))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})(o))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})(o))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})(o))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})(o))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}})(o))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})(o))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})(o))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})(o))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}})(o))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})(o))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})(o))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})(o))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})(o))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})(o))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})(o))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})(o))(o)(_N)({drumroll:R(rt(Ss("\u{1F941}")(n)(u)(function(f){return Ct(f)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(f){return function(i){return mt(f)([Ot(it)(1)([le(ge)(i)(_t())])])}}))),toc:R($S),allpass:R(zE(u)(e)(n)),analyser:R(rS(u)(e)(n)),bandpass:R(nS(u)(e)(n)),constant:R(fS(u)(e)(n)),compression:R(aS(u)(e)(n)),convolution:R(cS(u)(e)(n)),delay:R(lS(u)(e)(n)),gain:R(_S(u)(e)(n)),highpass:R(pS(u)(e)(n)),highshelf:R(sS(u)(e)(n)),iirFilter:R(mS(u)(e)(n)),loopBuf:R(vS(u)(e)(n)),lowshelf:R(dS(u)(e)(n)),lowpass:R(DS(u)(e)(n)),notch:R(AS(u)(e)(n)),playBuf:R(gS(u)(e)(n)),peaking:R(yS(u)(e)(n)),microphone:R(bS(u)(e)(n)),pan:R(FS(u)(e)(n)),periodicOsc:R(kS(u)(e)(n)),recorder:R(SS(u)(e)(n)),sawtoothOsc:R(hS(u)(e)(n)),sinOsc:R(TS(u)(e)(n)),squareOsc:R(xS(u)(e)(n)),triangleOsc:R(MS(u)(e)(n)),waveShaper:R(wS(u)(e)(n)),next:wa(N(c))(S(o))(n)(a)})}}}};var _b=function(){function t(){}return t.value=new t,t}(),OS={attr:function(t){return function(e){return b({key:"checked",value:L(e)})}}};var Eo=function(){function t(){}return t.value=new t,t}();var jo={attr:function(t){return function(e){return b({key:"type",value:L(e)})}}};var So=function(t){return function(e){return function(r){return new U(J(t)("input")(e)(new H(r)))}}};var vN=function(t){return t},Os=function(t){return function(e){return function(r){return io(t)(O(t.Plus0().Alt0())(Y(t)(e))(r))}}};var O_=function(t){return function(e){return t(e)}},Yi=function(t){return{map:function(e){return function(r){return function(n){return r(p(t)(function(a){return function(u){return a(e(u))}})(n))}}}}},gi=function(t){return function(e){return function(r){return function(n){return O_(p(Yi(t.Filterable1().Functor1()))(e)(r))(p(t.Filterable1().Functor1())(zf)(n))}}}};var Hc=function(t){return gi(t)(T)};var nu=vN;var IS=function(t){return function(e){return nu(function(r){return br(S(o))(O(N(c))(Y(S(o))(O_(t)(r)))(p(k)(function(n){return O_(n)(r)})(e)))})}},pb=function(t){return{apply:function(e){return function(r){return function(n){return r(e(p(t)(ho(Yo))(n)))}}},Functor0:function(){return Yi(t)}}};var zc=function(t){return function(e){return qt(function(r){return St(e)(function(n){return function(){var u=$_(t)();return r({acTime:u,value:n})()}})})}};var RS=function(t){return function(e){return function(r){var n=function(a){return function(u){return function(f){return function(i){return function(m){return function(s){return function(){var v=wr(f)();return $n(c)(v)(function(){var l=$_(t)(),C=dp(Ak($u(Na)(u-l-.04)(.01)*1e3))(function(){var vt=wr(f)();return $n(c)(vt)(function(){return Mn(u)(m)(),a(u)(),n(a)(u+s)(f)(i)(m)(s)()})()})();return Mn(new B(C))(i)()})()}}}}}}};return qt(function(a){return function(){var f=_r(!0)(),i=_r(V.value)(),m=$_(t)(),s=_r(m+e)();n(a)(e)(f)(i)(s)(e)();var _=St(r)(function(v){return function(){q(hr)(wr(i))(nr(c)(Je)(wl))();var l=wr(s)();return n(a)(l+v)(f)(i)(s)(v)()}})();return j(tt)(j(tt)(_)(Mn(!1)(f)))(q(hr)(wr(i))(nr(c)(Je)(wl)))}})}}};var Oa=function(t){return function(e){return function(r){return function(n){return function(a){var u=r===t||n===e;if(u)return e;var f=(n-e)/(r-t),i=e-f*t;return f*a+i}}}}};var DN=function(){return d.value}(),NS=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<section>
  <h2>Fold</h2>

  <p>The type of <code>fold</code> is:</p>

  <pre><code>fold
    :: forall event a b
    . IsEvent event
    => (a -> b -> b)
    -> event a
    -> b
    -> event b</code></pre>

  <p>Fold starts with some initial state <code>b</code> and, based on incoming events, allows you to change the state.</p>

  <p>One way <code>fold</code> is useful is to retain when certain actions happen. In the following example, we use <code>requestAnimationFrame</code> to animate the audio and we use four <code>fold</code>-s to store the ambitus and velocity of both vibrato and tremolo.</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p><code>fold</code> is so powerful because it allows us to localize state to <i>any</i> event. In the example above, instead of having a global state, our two folds allow for two <i>ad hoc</i> local states.</p>

</section>`}})()()(I()(I()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})(o))(o)(DN)({txt:R(or(ue)(`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap)
import Data.Tuple.Nested ((/\\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (blank, text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Behavior (sampleBy, sample_, step)
import FRP.Event (memoize)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (bang, fold, mapAccum, sampleOn)
import FRP.Event.VBus (V, vbus)
import Math (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (gain, periodicOsc)
import WAGS.Interpret (close, constant0Hack, context)
import WAGS.Math (calcSlope)
import WAGS.Core (AudioNumeric(..), _linear, bangOn)
import WAGS.Properties as P
import WAGS.Run (run2e)

type Cbx = V (cbx0 :: Unit, cbx1 :: Unit, cbx2 :: Unit, cbx3 :: Unit)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V
  ( startStop :: StartStop
  , cbx :: Cbx
  )

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        startE = bang unit <|> event.startStop.start
        stopE = event.startStop.stop
        chkState e = step false $ fold (const not) e false
        cbx0 = chkState event.cbx.cbx0
        cbx1 = chkState event.cbx.cbx1
        cbx2 = chkState event.cbx.cbx2
        cbx3 = chkState event.cbx.cbx3
      D.div_
        [ D.button
            ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                [ startE $> do
                    ctx <- context
                    c0h <- constant0Hack ctx
                    let
                      cevt fast b tm = mapAccum
                        ( \\(oo /\\ act) (pact /\\ pt) ->
                            let
                              tn = pt +
                                ( (act - pact) *
                                    (if oo then fast else 1.0)
                                )
                            in
                              ((act /\\ tn) /\\ tn)
                        )
                        (sampleBy (/\\) b tm)
                        (0.0 /\\ 0.0)

                    r <- run2e ctx
                      ( memoize
                          ( map (add 0.04 <<< _.acTime)
                              $ withACTime ctx animationFrameEvent
                          )
                          \\acTime ->
                            let
                              ev0 = cevt 8.0 cbx0 acTime
                              ev1 = map (if _ then 4.0 else 1.0) $ sample_ cbx1 acTime
                              ev2 = cevt 4.0 cbx2 acTime
                              ev3 = map (if _ then 4.0 else 1.0) $ sample_ cbx3 acTime
                              evs f a = sampleOn acTime
                                $ map ($)
                                $ sampleOn a
                                $ { f: _, a: _, t: _ } <$> f
                            in
                              [ gain 0.0
                                  ( evs ev0 ev1 <#> \\{ f, a, t } -> P.gain $ AudioNumeric
                                      { n: calcSlope 1.0 0.01 4.0 0.15 a * sin (pi * f) + 0.15
                                      , o: t
                                      , t: _linear
                                      }
                                  )
                                  [ periodicOsc
                                      { frequency: 325.6
                                      , spec: (0.3 +> -0.1 +> 0.7 +> -0.4 +> V.empty)
                                          /\\ (0.6 +> 0.3 +> 0.2 +> 0.0 +> V.empty)
                                      }
                                      ( oneOf
                                          [ bangOn
                                          , evs ev2 ev3 <#> \\{ f, a, t } -> P.frequency
                                              $ AudioNumeric
                                                { n: 325.6 +
                                                    (calcSlope 1.0 3.0 4.0 15.5 a * sin (pi * f))
                                                , o: t
                                                , t: _linear
                                                }
                                          ]
                                      )
                                  ]
                              ]
                      )
                    push.startStop.stop (r *> c0h *> close ctx)
                , stopE <#> (_ *> push.startStop.start unit)
                ]
            )
            [ text $ oneOf
                [ startE $> "Turn on"
                , stopE $> "Turn off"
                ]
            ]
        , D.div
            ( oneOfMap (map (attr D.Style))
                [ stopE $> "display:block;"
                , startE $> "display:none;"
                ]
            )
            ( map
                ( \\e -> D.input
                    ( oneOf
                        [ bang (D.Xtype := "checkbox")
                        , bang (D.OnClick := cb (const (e unit)))
                        , startE $> (D.Checked := "false")
                        ]
                    )
                    blank
                )
                ([ _.cbx0, _.cbx1, _.cbx2, _.cbx3 ] <@> push.cbx)
            )
        ]
  )`)),empl:R(rt(vu()(o)(Iu({reflectSymbol:function(){return"cbx"}})()()()(ur({reflectSymbol:function(){return"cbx0"}})()()(ur({reflectSymbol:function(){return"cbx1"}})()()(ur({reflectSymbol:function(){return"cbx2"}})()()(ur({reflectSymbol:function(){return"cbx3"}})()()(Hn)()()()())()()()())()()()())()()()())(Iu({reflectSymbol:function(){return"startStop"}})()()()(ur({reflectSymbol:function(){return"start"}})()()(ur({reflectSymbol:function(){return"stop"}})()()(Hn)()()()())()()()())(Hn)()()()())()()()())(d.value)(function(a){return function(u){var f=O(N(c))(Y(S(o))(void 0))(u.startStop.start),i=function(D){return Os(S(o))(!1)(Pu(S(o))(T(lu(Ga)))(D)(!1))},m=i(u.cbx.cbx3),s=i(u.cbx.cbx2),_=i(u.cbx.cbx1),v=i(u.cbx.cbx0);return Be(o)([Tn(o)(Nr(wt)(g(c))(p(k)(function(){var D=K(sr)(pr.value);return function(l){return D(Qe(T(l)))}}()))([tr(k)(On(S(o))(O(N(c))(Y(S(o))(h(c)(void 0)))(p(k)(function(D){return D.value0})(n)))(X(k)(f)(Z(et))))(function(D){return function(){D();var C=oa(ir)(),ot=zu(ir)(C)(),vt=function(he){return function(gr){return function(lt){return Pl(S(o))(function(oe){return function(ke){var Bt=ke.value1+(oe.value1-ke.value0)*function(){return oe.value0?he:1}();return new nt(new nt(oe.value1,Bt),Bt)}})(gi(S(o))(nt.create)(gr)(lt))(new nt(0,0))}}},Gt=Lc(C)(qi(o)(p(k)(function(){var he=Le(ka)(.04);return function(gr){return he(function(lt){return lt.acTime}(gr))}}())(zc(C)(Ki)))(function(he){var gr=function(vr){return function(Su){return io(S(o))(he)(p(k)(tl)(io(S(o))(Su)(p(k)(function(Vu){return function(xi){return function(au){return{f:Vu,a:xi,t:au}}}})(vr))))}},lt=p(k)(function(vr){return vr?4:1})(Hc(S(o))(m)(he)),oe=vt(4)(s)(he),ke=p(k)(function(vr){return vr?4:1})(Hc(S(o))(_)(he)),Bt=vt(8)(v)(he);return[Hr(it)(0)(tr(k)(gr(Bt)(ke))(function(vr){return Fn()(Lr)({n:Oa(1)(.01)(4)(.15)(vr.a)*Tp(Ji*vr.f)+.15,o:vr.t,t:Uo})}))([Di(vi(yt(At()(z(z(bt)(mi(pi(ua)))()()()({reflectSymbol:function(){return"spec"}}))(si)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:325.6,spec:new nt(we(Me(ua)()(An)()(Ha))(.3)(we(Me(Au)()(yn)()(An))(-.1)(we(Me(yu)()(kn)()(yn))(.7)(we(Me(ku)()(gu)()(kn))(-.4)(Uu)))),we(Me(ua)()(An)()(Ha))(.6)(we(Me(Au)()(yn)()(An))(.3)(we(Me(yu)()(kn)()(yn))(.2)(we(Me(ku)()(gu)()(kn))(0)(Uu)))))})(Ve(wt)(g(c))([_t(),tr(k)(gr(oe)(lt))(function(vr){return Co()(Lr)({n:325.6+Oa(1)(3)(4)(15.5)(vr.a)*Tp(Ji*vr.f),o:vr.t,t:Uo})})]))])]}))(),re=j(tt)(j(tt)(Gt)(ot))(xn(ir)(C));return t(j(tt)(re)(a.startStop.start(void 0)))(),a.startStop.stop(re)()}}),tr(k)(u.startStop.stop)(function(D){return j(tt)(D)(j(tt)(t(h(c)(void 0)))(a.startStop.start(void 0)))})]))([ln(ue)(Ve(wt)(g(c))([X(k)(f)("Turn on"),X(k)(u.startStop.stop)("Turn off")]))]),Te(o)(Nr(wt)(g(c))(p(k)(K(pt)(Jt.value)))([X(k)(u.startStop.stop)("display:block;"),X(k)(f)("display:none;")]))(p(xe)(function(D){return So(o)(Ve(wt)(g(c))([Y(S(o))(K(jo)(Eo.value)("checkbox")),Y(S(o))(K(sr)(pr.value)(Qe(T(D(void 0))))),X(k)(f)(K(OS)(_b.value)("false"))]))([])})(js(xe)([function(D){return D.cbx0},function(D){return D.cbx1},function(D){return D.cbx2},function(D){return D.cbx3}])(a.cbx)))])}})))})}}}};var sb={recip:function(t){return 1/t},Ring0:function(){return of}};var mb=function(t){return function(e){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return e}}}};function Vc(t){return function(){return function(e){return t(e)()}}}function Jc(t){return function(e){return function(r){return function(n){return function(){return n.addEventListener(t,e,r)}}}}}function jc(t){return function(e){return function(r){return function(n){return function(){return n.removeEventListener(t,e,r)}}}}}function vb(t){return t.clientX}function Db(t){return t.clientY}function I_(t){return t.button}var R_=Nt("MouseEvent");var LS=function(t){return function(e){return qt(function(r){return St(e)(function(n){return function(){var u=wr(t.buttons)();return r({value:n,buttons:u})()}})})}};var WS=function(){var e=_r(V.value)(),r=_r(Jm)(),n=p(x)(ib)(ki)(),a=Vc(function(m){return nr(c)(Je)(function(s){return Mn(new B({x:vb(s),y:Db(s)}))(e)})(R_(m))})(),u=Vc(function(m){return nr(c)(Je)(function(s){return jf(wy(jr)(I_(s)))(r)})(R_(m))})(),f=Vc(function(m){return nr(c)(Je)(function(s){return jf(Dp(jr)(I_(s)))(r)})(R_(m))})();Jc(fn()("mousemove"))(a)(!1)(n)(),Jc(fn()("mousedown"))(u)(!1)(n)(),Jc(fn()("mouseup"))(f)(!1)(n)();var i=function(){return jc(fn()("mousemove"))(a)(!1)(n)(),jc(fn()("mousedown"))(u)(!1)(n)(),jc(fn()("mouseup"))(f)(!1)(n)()};return{position:e,buttons:r,dispose:i}},GS=qt(function(t){return function(){var r=p(x)(ib)(ki)(),n=Vc(function(a){return nr(c)(Je)(function(u){return t(I_(u))})(R_(a))})();return Jc(fn()("mousedown"))(n)(!1)(r)(),jc(fn()("mousedown"))(n)(!1)(r)}});var US=function(t){return nu(function(e){return p(k)(function(r){return r.value(r.buttons)})(LS(t)(e))})};var Ab=function(t){return t};function Ns(){return Date.now()}var sh=function(t){return qt(function(e){return St(t)(function(r){return function(){var a=Ns();return e({time:a,value:r})()}})})};var YN=nu(function(t){return p(k)(function(e){return e.value(e.time)})(sh(t))}),kb=p(Yi(k))(function(){var t=g_(FE);return function(e){return t(Ab(e))}}())(YN);var t1=function(t){var e=function(u){return function(f){return function(i){return function(m){return function(s){return function(_){return function(v){var D=Le(f.DivisionRing1().Ring0().Semiring0())(ga(f.DivisionRing1().Ring0().Semiring0()))(ga(f.DivisionRing1().Ring0().Semiring0())),l=function(C){return function(ot){if(C.last instanceof V)return ot;if(C.last instanceof B)return Le(i)(ot)(m(function(vt){return Ku(f.EuclideanRing0())(Wn(f.DivisionRing1().Ring0().Semiring0())(vt(Le(i)(C.last.value0.value1)(C.now.value1)))(Fu(f.DivisionRing1().Ring0())(C.now.value0)(C.last.value0.value0)))(D)}));throw new Error("Failed pattern match at WAGS.Example.Docs.FixEx (line 102, column 5 - line 102, column 35): "+[C.constructor.name,ot.constructor.name])}};return nu(function(C){var ot=O_(v)(X(u.Filterable1().Functor1())(C)(Z(et))),vt=Ap(u)(gi(u)(nt.create)(_)(ot)),Gt=Pu(u)(l)(vt)(s);return io(u)(Gt)(C)})}}}}}}},r=function(u){return function(f){return e(u)(f)(f.DivisionRing1().Ring0().Semiring0())(function(i){return i(Z(et))})}},n=function(u){return function(f){return nu(function(i){return Ol(S(o))(function(m){var s=f(Os(S(o))(u)(m));return{input:Hc(S(o))(s)(i),output:io(S(o))(m)(i)}})})}},a=function(u){return function(f){return function(i){if(My(u))return-8*(f-1)-i*2;if(Ze)return 2*(4-f);throw new Error("Failed pattern match at WAGS.Example.Docs.FixEx (line 62, column 3 - line 64, column 34): "+[u.constructor.name,f.constructor.name,i.constructor.name])}}};return n(2)(function(u){return r(S(o))(mb(cl)(sb))(2)(p(Yi(k))(wn())(kb))(function(){var f=n(10)(function(i){return r(S(o))(mb(cl)(sb))(10)(p(Yi(k))(wn())(kb))(zt(pb(k))(zt(pb(k))(p(Yi(k))(a)(US(t)))(u))(i))});return IS(f)(X(k)(GS)(f))}())})},e1=function(){return d.value}(),mh=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in wags that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(I()(I()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})(o))(o)(e1)({txt:R(or(ue)(`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (second)
import Data.Set (isEmpty)
import Data.Tuple.Nested ((/\\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb)
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random (randomInt)
import FRP.Behavior (ABehavior, Behavior, behavior, sample, sampleBy, sample_, step, switcher)
import FRP.Behavior.Mouse (buttons)
import FRP.Behavior.Time as Time
import FRP.Event (memoize)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (class IsEvent, bang, fix, fold, sampleOn, withLast)
import FRP.Event.Mouse (Mouse, down, getMouse)
import FRP.Event.VBus (V, vbus)
import Test.QuickCheck (arbitrary, mkSeed)
import Test.QuickCheck.Gen (evalGen)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (bandpass_, gain, lowpass_, periodicOsc, squareOsc_)
import WAGS.Interpret (close, constant0Hack, context)
import WAGS.Core (AudioNumeric(..), _linear, bangOn)
import WAGS.Properties as P
import WAGS.Run (run2e)

type StartStop = V (start :: Unit, stop :: Effect Unit)

-- \`swell\` is an interactive function of time defined by a differential equation:
--
-- d^2s/dt^2
--   | mouse down = \u237A - \u03B2s
--   | mouse up   = \u0263 - \u03B4s - \u03B5 ds/dt
--
-- So the function exhibits either decay or growth depending on if
-- the mouse is pressed or not.
--
-- We can solve the differential equation by integration using \`solve2'\`.
swell :: Mouse -> Behavior Number
swell mouse =
  fixB 2.0 \\b ->
    integral' 2.0 (unwrap <$> Time.seconds)
      let
        db = fixB 10.0 \\db_ ->
          integral' 10.0 (unwrap <$> Time.seconds) (f <$> buttons mouse <*> b <*> db_)
      in
        switcher db (down $> db)
  where
  f bs s ds
    | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
    | otherwise = 2.0 * (4.0 - s)

  fixB :: forall a. a -> (Behavior a -> Behavior a) -> Behavior a
  fixB a fn = behavior \\s ->
    fix \\event ->
      let
        b = fn (step a event)
      in
        { input: sample_ b s, output: sampleOn event s }

  -- | Integrate with respect to some measure of time.
  -- |
  -- | This function approximates the integral using the trapezium rule at the
  -- | implicit sampling interval.
  -- |
  -- | The \`Semiring\` \`a\` should be a vector field over the field \`t\`. To represent
  -- | this, the user should provide a _grate_ which lifts a multiplication
  -- | function on \`t\` to a function on \`a\`. Simple examples where \`t ~ a\` can use
  -- | the \`integral'\` function instead.
  integral
    :: forall event a t
     . IsEvent event
    => Field t
    => Semiring a
    => (((a -> t) -> t) -> a)
    -> a
    -> ABehavior event t
    -> ABehavior event a
    -> ABehavior event a
  integral g initial t b =
    behavior \\e ->
      let
        x = sample b (e $> identity)
        y = withLast (sampleBy (/\\) t x)
        z = fold approx y initial
      in
        sampleOn z e
    where
    approx { last: Nothing } s = s
    approx { now: t1 /\\ a1, last: Just (t0 /\\ a0) } s = s + g (\\z -> z (a0 + a1) * (t1 - t0) / two)

    two :: t
    two = one + one

  -- | Integrate with respect to some measure of time.
  -- |
  -- | This function is a simpler version of \`integral\` where the function being
  -- | integrated takes values in the same field used to represent time.
  integral'
    :: forall event t
     . IsEvent event
    => Field t
    => t
    -> ABehavior event t
    -> ABehavior event t
    -> ABehavior event t
  integral' = integral (_ $ identity)

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ StartStop) \\push event -> do
      let
        startE = bang unit <|> event.start
        stopE = event.stop
      D.div_
        [ D.button
            ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                [ startE $>
                    do
                      ctx <- context
                      c0h <- constant0Hack ctx
                      mouse <- getMouse
                      ri <- randomInt 0 10000
                      let
                        ttap (o /\\ n) = AudioNumeric { o: o + 0.04, n, t: _linear }
                        fund = 90.4
                        spcn = map (_ - 0.5) arbitrary
                        spc' = do
                          a <- spcn
                          b <- spcn
                          c <- spcn
                          d <- spcn
                          pure (a +> b +> c +> d +> V.empty)
                        spc = (/\\) <$> spc' <*> spc'
                        spcs = { s0: _, s1: _, s2: _, s3: _ } <$> spc <*> spc <*> spc <*> spc
                        allSpcs = evalGen spcs { newSeed: mkSeed ri, size: 5 }
                      r <- run2e ctx
                        ( memoize
                            ( map (\\{ acTime, value } -> acTime /\\ value)
                                $ withACTime ctx
                                $ sample_ (swell mouse) animationFrameEvent
                            )
                            \\swm ->
                              [ gain 0.0
                                  ( P.gain
                                      <<< ttap
                                      <<< second (\\x -> max (-0.4) $ 0.5 * (x - 1.0)) <$> swm
                                  )
                                  [ lowpass_ { frequency: fund, q: 20.0 }
                                      [ squareOsc_ fund ]
                                  ]
                              , gain 0.0
                                  ( P.gain
                                      <<< ttap
                                      <<< second (\\x -> max (-0.2) $ 0.4 * (x - 3.0)) <$> swm
                                  )
                                  [ bandpass_ { frequency: fund * 4.0, q: 20.0 }
                                      [ periodicOsc
                                          { frequency: (fund * 3.02)
                                          , spec: allSpcs.s0
                                          }
                                          ( bangOn <|>
                                              ( P.frequency
                                                  <<< ttap
                                                  <<< second (\\x -> fund * 3.02 + 14.0 * (x - 1.0)) <$> swm
                                              )
                                          )
                                      ]
                                  ]
                              , gain 0.0
                                  ( P.gain
                                      <<< ttap
                                      <<< second (\\x -> max (-0.1) $ 0.2 * (x - 6.0)) <$> swm
                                  )
                                  [ bandpass_ { frequency: fund * 6.0, q: 20.0 }
                                      [ periodicOsc
                                          { frequency: fund * 5.07
                                          , spec: allSpcs.s1
                                          }
                                          ( bangOn <|>
                                              ( P.frequency
                                                  <<< ttap
                                                  <<< second (\\x -> fund * 5.07 + 18.0 * (x - 1.0)) <$> swm
                                              )
                                          )
                                      ]
                                  ]
                              , gain 0.0
                                  ( P.gain
                                      <<< ttap
                                      <<< second (\\x -> max 0.0 $ 0.2 * (x - 3.0)) <$> swm
                                  )
                                  [ bandpass_ { frequency: fund * 8.0, q: 20.0 }
                                      [ periodicOsc
                                          { frequency: fund * 7.13
                                          , spec: allSpcs.s2
                                          }
                                          ( bangOn <|>
                                              ( P.frequency
                                                  <<< ttap
                                                  <<< second (\\x -> fund * 7.13 + 32.0 * (x - 1.0)) <$> swm
                                              )
                                          )
                                      ]
                                  ]
                              , gain 0.0
                                  ( P.gain
                                      <<< ttap
                                      <<< second (\\x -> max 0.0 $ 0.1 * (x - 7.0)) <$> swm
                                  )
                                  [ periodicOsc
                                      { frequency: fund * 9.14
                                      , spec: allSpcs.s3
                                      }
                                      ( bangOn <|>
                                          ( P.frequency
                                              <<< ttap
                                              <<< second (\\x -> fund * 9.14 + 31.0 * (x - 1.0)) <$> swm
                                          )
                                      )
                                  ]
                              ]
                        )
                      push.stop (r *> c0h *> close ctx)
                , event.stop <#> (_ *> push.start unit)
                ]
            )
            [ text $ oneOf
                [ startE $> "Turn on"
                , stopE $> "Turn off"
                ]
            ]
        ]
  )`)),empl:R(rt(vu()(o)(ur({reflectSymbol:function(){return"start"}})()()(ur({reflectSymbol:function(){return"stop"}})()()(Hn)()()()())()()()())(d.value)(function(a){return function(u){var f=O(N(c))(Y(S(o))(void 0))(u.start);return Be(o)([Tn(o)(Nr(wt)(g(c))(p(k)(function(){var i=K(sr)(pr.value);return function(m){return i(Qe(T(m)))}}()))([tr(k)(On(S(o))(O(N(c))(Y(S(o))(h(c)(void 0)))(p(k)(function(i){return i.value0})(n)))(X(k)(f)(Z(et))))(function(i){return function(){i();var s=oa(ir)(),_=zu(ir)(s)(),v=WS(),D=Hl(0)(1e4)(),l=function(lt){return{o:lt.value0+.04,n:lt.value1,t:Uo}},C=p(lo)(function(lt){return lt-.5})(t_(jk)),ot=q(hf)(C)(function(lt){return q(hf)(C)(function(oe){return q(hf)(C)(function(ke){return q(hf)(C)(function(Bt){return h(Yl)(we(Me(ua)()(An)()(Ha))(lt)(we(Me(Au)()(yn)()(An))(oe)(we(Me(yu)()(kn)()(yn))(ke)(we(Me(ku)()(gu)()(kn))(Bt)(Uu)))))})})})}),vt=zt(Tf)(p(lo)(nt.create)(ot))(ot),Gt=zt(Tf)(zt(Tf)(zt(Tf)(p(lo)(function(lt){return function(oe){return function(ke){return function(Bt){return{s0:lt,s1:oe,s2:ke,s3:Bt}}}}})(vt))(vt))(vt))(vt),re=vc(Gt)({newSeed:_c(D),size:5}),he=Lc(s)(qi(o)(p(k)(function(lt){return new nt(lt.acTime,lt.value)})(zc(s)(Hc(S(o))(t1(v))(Ki))))(function(lt){return[Hr(it)(0)(p(k)(function(){var oe=Fn()(Lr),ke=Xn(gn)(function(Bt){return $u(Na)(-.4)(.5*(Bt-1))});return function(Bt){return oe(l(ke(Bt)))}}())(lt))([$c(FD(yt(At()(z(z(bt)(EC)()()()({reflectSymbol:function(){return"q"}}))(dD)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:90.4,q:20})([cE(yc)(90.4)])]),Hr(it)(0)(p(k)(function(){var oe=Fn()(Lr),ke=Xn(gn)(function(Bt){return $u(Na)(-.2)(.4*(Bt-3))});return function(Bt){return oe(l(ke(Bt)))}}())(lt))([hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:90.4*4,q:20})([Di(vi(yt(At()(z(z(bt)(mi(pi(ua)))()()()({reflectSymbol:function(){return"spec"}}))(si)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:90.4*3.02,spec:re.s0})(O(N(c))(_t())(p(k)(function(){var oe=Co()(Lr),ke=Xn(gn)(function(Bt){return 90.4*3.02+14*(Bt-1)});return function(Bt){return oe(l(ke(Bt)))}}())(lt)))])]),Hr(it)(0)(p(k)(function(){var oe=Fn()(Lr),ke=Xn(gn)(function(Bt){return $u(Na)(-.1)(.2*(Bt-6))});return function(Bt){return oe(l(ke(Bt)))}}())(lt))([hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:90.4*6,q:20})([Di(vi(yt(At()(z(z(bt)(mi(pi(ua)))()()()({reflectSymbol:function(){return"spec"}}))(si)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:90.4*5.07,spec:re.s1})(O(N(c))(_t())(p(k)(function(){var oe=Co()(Lr),ke=Xn(gn)(function(Bt){return 90.4*5.07+18*(Bt-1)});return function(Bt){return oe(l(ke(Bt)))}}())(lt)))])]),Hr(it)(0)(p(k)(function(){var oe=Fn()(Lr),ke=Xn(gn)(function(Bt){return $u(Na)(0)(.2*(Bt-3))});return function(Bt){return oe(l(ke(Bt)))}}())(lt))([hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:90.4*8,q:20})([Di(vi(yt(At()(z(z(bt)(mi(pi(ua)))()()()({reflectSymbol:function(){return"spec"}}))(si)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:90.4*7.13,spec:re.s2})(O(N(c))(_t())(p(k)(function(){var oe=Co()(Lr),ke=Xn(gn)(function(Bt){return 90.4*7.13+32*(Bt-1)});return function(Bt){return oe(l(ke(Bt)))}}())(lt)))])]),Hr(it)(0)(p(k)(function(){var oe=Fn()(Lr),ke=Xn(gn)(function(Bt){return $u(Na)(0)(.1*(Bt-7))});return function(Bt){return oe(l(ke(Bt)))}}())(lt))([Di(vi(yt(At()(z(z(bt)(mi(pi(ua)))()()()({reflectSymbol:function(){return"spec"}}))(si)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:90.4*9.14,spec:re.s3})(O(N(c))(_t())(p(k)(function(){var oe=Co()(Lr),ke=Xn(gn)(function(Bt){return 90.4*9.14+31*(Bt-1)});return function(Bt){return oe(l(ke(Bt)))}}())(lt)))])]}))(),gr=j(tt)(j(tt)(he)(_))(xn(ir)(s));return t(j(tt)(gr)(a.start(void 0)))(),a.stop(gr)()}}),tr(k)(u.stop)(function(i){return j(tt)(i)(j(tt)(t(h(c)(void 0)))(a.start(void 0)))})]))([ln(ue)(Ve(wt)(g(c))([X(k)(f)("Turn on"),X(k)(u.stop)("Turn off")]))])])}})))})}}}};var n1=function(){return d.value}(),vh=function(t){return function(e){return function(r){return function(n){var a=Pa(t)(r);return $t({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(I()(I()(_n()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}})(o))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}})(o))(o)(n1)({next:wa(N(c))(S(o))(n)(j(tt)(e(x_.value))(vn)),fold:R(NS(a)(e)(r)(n)),fix:R(mh(a)(e)(r)(n))})}}}};var u1=function(){function t(){}return t.value=new t,t}(),Dh=function(){function t(){}return t.value=new t,t}(),gb=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),o1=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Profunctor (lcmap)
import Deku.Attribute (cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody, runInBody1)
import Effect (Effect)
import FRP.Event (bus)
import FRP.Event.Class (bang)
import Math (pow)
import WAGS.Control (gain_, gain, sinOsc)
import WAGS.Core (AudioEnvelope(..), AudioOnOff(..), _on, _off)
import WAGS.Properties (onOff)
import WAGS.Properties as P
import WAGS.Run (run2_)

data UIEvents = Init | Start | Stop (Effect Unit)

-- an event to turn our oscillators on
oon o = bang $ onOff $ AudioOnOff { x: _on, o }
-- an event to turn our oscillators off
oof o = bang $ onOff $ AudioOnOff { x: _off, o }
-- an event with an envelope for our gain
env o = bang $ P.gain
  $ AudioEnvelope
      { p: [ 0.0, 0.4, 0.1, 0.05, 0.01, 0.0 ]
      , d: 0.8
      , o
      }

-- a single cell with four oscillators,
-- each of which have the envelope applied
cell = lcmap toNumber \\i -> do
  let
    ooo' x = oon (x + 0.27 * (i * (1.005 \`pow\` i)))
      <|> oof (x + 3.0 + 0.3 * (i * (1.005 \`pow\` i)))
    env' x = env (x + 0.3 * (i * (1.005 \`pow\` i)))
    strand x y =
      gain 0.0 (env' x) [ sinOsc (200.0 + i * y) (ooo' x) ]
  [ strand 0.2 4.0
  , strand 0.3 6.0
  , strand 0.45 14.0
  , strand 0.7 20.0
  ]

main :: Effect Unit
main = runInBody1 (bus \\push -> lcmap (bang Init <|> _) \\event ->
  D.div_
    [ D.button
        ( event <#>
            \\e -> D.OnClick := cb
              ( const $ case e of
                  Stop u -> u *> push Start
                  _ -> do
                    r <- run2_
                      [ gain_ 1.0
                          -- we create 100 cells
                          $ join
                          $ cell <$> 0 .. 100
                      ]
                    push $ Stop r
              )
        )
        [ text $ event <#> case _ of
            Stop _ -> "Turn off"
            _ -> "Turn on"
        ]
    ])
`;var i1=function(){return d.value}(),f1=function(t){return function(e){return function(r){return Y(t)(Bf(e)(Mf)({x:vD,o:r}))}}},c1=function(t){return function(e){return function(r){return Y(t)(Bf(e)(Mf)({x:pC,o:r}))}}},l1=oo(Ta)(qe)(function(t){var e=function(a){return O(N(c))(f1(S(o))()(a+.27*(t*Vi(1.005)(t))))(c1(S(o))()(a+3+.3*(t*Vi(1.005)(t))))},r=function(a){return Y(S(o))(Fn()(Rn)({p:[0,.4,.1,.05,.01,0],o:a+.3*(t*Vi(1.005)(t)),d:.8}))},n=function(a){return function(u){return Hr(it)(0)(r(a))([Rf(Xi)(200+t*u)(e(a))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),dh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2>Example 1: Hello events</h2>

  <p>Let's say hi to events! The simplest of events, which we've seen already, are the ones that occur <span style="font-weight:800;">now</span>, that is to say, immediately upon subscription. You create those types of events using <code>bang</code>. In this section, we'll use <code>bang</code> to set several different types of values:</p>

  <ul>
    <li><code>AudioEnvelope</code> to create an envelope for the gain node. To construct one, use a record with the following parameters:<ul><li><code>p</code>: a list of numbers that will be interpolated over.</li><li><code>o</code>: the offset in time from the AudioContext clock's start time.</li><li><code>d</code>: the duration of the envelope.</li></ul></li>
    <li><code>AudioOnOff</code> to turn the sine-wave oscillator on and off. To construct one, use a record with the following parameters:<ul><li><code>n</code>: an enum with the value <code>_on</code>, <code>_off</code> or <code>_onOff</code> (more on this in <a href="#example3">Example 3</a> below).</li><li><code>o</code>: the offset in time from the AudioContext clock's start time.</li></ul></li>
  </ul>

  <p>After that, in the example below, it's functions all the way down. <code>oon</code> and <code>oof</code> create our on/off events, <code>env</code> creates our gain envelope, <code>ooo'</code> and <code>env'</code> specialize these envelopes to a specific point in time, and <code>cell</code> creates a single cell that we deploy 100 times.</p>

  <p>One important thing to note here is the use of the tie fighter (<code>&lt;|&gt;</code>), aka <code>alt</code>, in the definition of <code>ooo'</code>. The <code>Event</code> type, when <code>alt</code>'d, preserves a before-after relationship of the left and right operands when the operands happen at the same time. This is a bit hackish: the events conceptually happen at the same time, but on our CPU, one has to follow the other. We can use this, however, to make sure that certain events happen in a logical sequence. For example, an <code>off</code> instruction must be issued after an <code>on</code> instruction, which we guarantee by using <code>oon</code> on the left side of the alt. If we did it the other way, the <code>on</code> instruction would be last and we'd wind up with 100 oscillators playing at the same time!</p>

  <p>A last thing to note before the music plays is how scheduling works here. Even though all the events are issued upfront via <code>bang</code>, they schedule things to be played <i>later</i> in the audio context. We'll see more advanced scheduling techniques in the <a href="#example4"><code>requestAnimationFrame</code> example below</a>.</p>

  <pre><code>@txt@</code></pre>

  @ex0@

  <p>Unlike the previous examples, this one and all subsequent ones are "batteries included", meaning they are single-file, self-contained PureScript examples that you can compile and run yourself.</p>

</section>
`}})({reflectType:function(){return"@"}})()()(o)(I()(I()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})(o))(d.value)(i1)({txt:R(or(ue)(o1)),ex0:R(rt(Zr(o)(function(n){return oo(Ta)(function(a){return O(N(c))(Y(S(o))(u1.value))(a)})(function(a){return Be(o)([Tn(o)(tr(k)(On(S(o))(O(N(c))(Y(S(o))(h(c)(void 0)))(p(k)(function(u){return u.value0})(r)))(p(k)(nt.create)(a)))(function(u){return K(sr)(pr.value)(Qe(T(function(){return u.value0 instanceof gb?j(tt)(j(tt)(u.value0.value0)(n(Dh.value)))(t(h(c)(void 0))):function(){u.value1();var i=hs([Ot(it)(1)(Zn(Zo)(p(xe)(l1)(dn(0)(100))))])();return t(j(tt)(i)(n(Dh.value)))(),n(new gb(i))()}}())))}))([ln(ue)(tr(k)(a)(function(u){return u instanceof gb?"Turn off":"Turn on"}))])])})})))})}}};var Ci=function(){function t(){}return t.value=new t,t}();var tf={attr:function(t){return function(e){return b({key:"max",value:L(e)})}}};var Ei=function(){function t(){}return t.value=new t,t}();var ef={attr:function(t){return function(e){return b({key:"min",value:L(e)})}}};var Si=function(){function t(){}return t.value=new t,t}();var rf={attr:function(t){return function(e){return b({key:"input",value:ct(e)})}}};var hi=function(){function t(){}return t.value=new t,t}(),nf={attr:function(t){return function(e){return b({key:"step",value:L(e)})}}};var Ti=function(){function t(){}return t.value=new t,t}();var af={attr:function(t){return function(e){return b({key:"value",value:L(e)})}}};function bh(t){return t.target}var Xc=function(t){return tn(bh(t))};var m1=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (blank, plant, switcher, text, text_)
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create)
import FRP.Event.Class (bang, biSampleOn, keepLatest)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf)
import WAGS.Interpret (bracketCtx, decodeAudioDataFromUri)
import WAGS.Math (calcSlope)
import WAGS.Core (bangOn)
import WAGS.Properties (loopEnd, loopStart, playbackRate)
import WAGS.Run (run2_)
import WAGS.WebAPI (BrowserAudioBuffer)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type Slider = V (s0 :: Number, s1 :: Number, s2 :: Number)
type StartStop = V (start :: Unit, stop :: Effect Unit)
type UIEvents = V (startStop :: StartStop, slider :: Slider)

atari =
  "https://freesound.org/data/previews/100/100981_1234256-lq.mp3" :: String

main :: Effect Unit
main = do
  { push, event } <- create
  runInBody (switcher scene event)
  push Nothing
  launchAff_ $ bracketCtx
    \\ctx -> decodeAudioDataFromUri ctx atari >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Element lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ keepLatest $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        sl0 = event.slider.s0
        sl1 = event.slider.s1
        sl2 = event.slider.s2
        start = event.startStop.start <|> bang unit
        music = run2_
          [ loopBuf
              { buffer: buffer
              , playbackRate: 2.6
              , loopStart: 0.6
              , loopEnd: 1.1
              }
              $ oneOf
                [ bangOn
                , map
                    (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate)
                    sl0
                , map
                    (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart)
                    sl1
                , map
                    (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd)
                    (biSampleOn sl2 (add <$> (bang 0.0 <|> sl1)))
                ]
          ]
      plant $ D.div_
        $
          map
            ( \\{ l, f } -> D.div_
                [ text_ l
                , D.input
                    ( oneOfMap bang
                        [ D.Xtype := "range"
                        , D.Min := "0"
                        , D.Max := "100"
                        , D.Step := "1"
                        , D.Value := "50"
                        , D.OnInput := cb
                            ( traverse_
                                (valueAsNumber >=> f)
                                <<< (=<<) fromEventTarget
                                <<< target
                            )
                        ]
                    )
                    blank
                ]
            )
            [ { l: "Playback rate", f: push.slider.s0 }
            , { l: "Loop start", f: push.slider.s1 }
            , { l: "Loop end", f: push.slider.s2 }
            ] <>
            [ D.button
                ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                    [ start $> (music >>= push.startStop.stop)
                    , event.startStop.stop <#>
                        (_ *> push.startStop.start unit)
                    ]
                )
                [ text $ oneOf
                    [ start $> "Turn on"
                    , event.startStop.stop $> "Turn off"
                    ]
                ]
            ]
`,v1=function(){return d.value}(),D1="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",Ah=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
 <h2>Example 2: Three sliders</h2>

  <p>In this example, we'll use three sliders to control the playback rate, the start time, and the end time of a looping buffer.</p>

  <p>There is a fair bit of DOM-related code in this example, so before showing the whole thing, let's isolate the Wags bit.</p>

  <pre><code>@wagtxt@</code></pre>

  <p>Note that our loopBuf consumes four events: in addition to the three sliders, there is a <code>bangOn</code> event that turns it on. For the events belonging to range sliders, we use <code>calcSlope</code> to normalize the range to sensible values for these parameters.</p>

  <p>Because each slider event contains a number, we can compose it with a function from <code>WAGS.Properties</code>, like <code>playbackRate</code> or <code>loopStart</code>, to create an event that controls a Wags parameter. The <code>oneOf</code> directive indicates that the incoming event will be "one of" the events in the array. It's also possible to use the tie-fighter, aka <code>alt</code>, to separate each event, but I like the array syntax when possible as tie fighters do, after all, work for the Empire, and who likes the Empire?</p>

  <p>And below you'll find the full example. It also shows useful patterns like downloading audio files and filtering events.</p>

  <pre><code>@txt@</code></pre>

  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(o)(I()(I()(I()(Q(c))({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}})(o))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(o))(d.value)(v1)({wagtxt:R(or(ue)(`run2_
  $ loopBuf
      { buffer: buffer
      , playbackRate: 2.6
      , loopStart: 0.6
      , loopEnd: 1.1
      }
  $ oneOf
      [ bangOn
      , (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate) <$> sl0
      , (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart) <$> sl1
      , (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd) <$> biSampleOn sl2
          (add <$> (bang 0.0 <|> sl1))
      ]`)),txt:R(or(ue)(m1)),ex1:R(rt(vu()(o)(Iu({reflectSymbol:function(){return"slider"}})()()()(ur({reflectSymbol:function(){return"s0"}})()()(ur({reflectSymbol:function(){return"s1"}})()()(ur({reflectSymbol:function(){return"s2"}})()()(Hn)()()()())()()()())()()()())(Iu({reflectSymbol:function(){return"startStop"}})()()()(ur({reflectSymbol:function(){return"loading"}})()()(ur({reflectSymbol:function(){return"start"}})()()(ur({reflectSymbol:function(){return"stop"}})()()(Hn)()()()())()()()())()()()())(Hn)()()()())()()()())(d.value)(function(n){return function(a){var u=O(N(c))(a.startStop.start)(Y(S(o))(void 0)),f=function(i){return le(Of(yt(At()(z(z(z(z(bt)(hc)()()()({reflectSymbol:function(){return"playbackRate"}}))(m_)()()()({reflectSymbol:function(){return"loopStart"}}))(s_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Pf)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())))({buffer:i,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(Ve(wt)(g(c))([_t(),p(k)(function(){var m=ia()(os),s=Oa(0)(.2)(100)(5);return function(_){return m(s(_))}}())(a.slider.s0),p(k)(function(){var m=oS(),s=Oa(0)(0)(100)(1.2);return function(_){return m(s(_))}}())(a.slider.s1),p(k)(function(){var m=iS(),s=Oa(0)(.05)(100)(1);return function(_){return m(s(_))}}())(On(S(o))(a.slider.s2)(p(k)(Le(ka))(O(N(c))(Y(S(o))(0))(a.slider.s1))))]))};return Be(o)(gt(Ra)(p(xe)(function(i){return Be(o)([or(ue)(i.l),So(o)(Nr(wt)(g(c))(Y(S(o)))([K(jo)(Eo.value)("range"),K(ef)(Ei.value)("0"),K(tf)(Ci.value)("100"),K(nf)(hi.value)("1"),K(af)(Ti.value)("50"),K(rf)(Si.value)(Qe(function(){var m=nr(c)(Je)(Vf(hr)(Gf)(i.f)),s=Yn(Ea)(Uc);return function(_){return m(s(Xc(_)))}}()))]))([])])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([Tn(o)(Nr(wt)(g(c))(p(k)(function(){var i=K(sr)(pr.value);return function(m){return i(Qe(T(m)))}}()))([X(k)(a.startStop.loading)(h(c)(void 0)),tr(k)(a.startStop.stop)(function(i){return j(tt)(i)(j(tt)(t(h(c)(void 0)))(n.startStop.start(void 0)))}),tr(k)(On(S(o))(O(N(c))(Y(S(o))(h(c)(void 0)))(p(k)(function(i){return i.value0})(r)))(X(k)(u)(Z(et))))(function(i){return function(){i(),n.startStop.loading(void 0)();var s=go(q(Rr)(oa(Ir))(function(_){return q(Rr)(zu(Ir)(_))(function(v){return q(Rr)(Ct(_)(D1))(function(D){return mr(Ir)(function(){var C=mt(_)([f(D)])(),ot=j(tt)(j(tt)(C)(v))(xn(ir)(_));return n.startStop.stop(ot)(),ot})})})}))();return t(function(){return n.startStop.start(void 0)(),zo(Ai(s))()})(),void 0}})]))([ln(ue)(Ve(wt)(g(c))([p(k)(T("Turn off"))(a.startStop.stop),p(k)(T("Turn on"))(u)]))])]))}})))})}}};var b1=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (blank, text, text_)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (Event, makeEvent, memoize, subscribe)
import FRP.Event.Class (bang)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import WAGS.Clock (interval)
import WAGS.Control (bandpass_, fan1, gain, gain_, highpass_, triangleOsc)
import WAGS.Core (Node)
import WAGS.Interpret (close, context)
import WAGS.Math (calcSlope)
import WAGS.Core (AudioEnvelope(..), bangOn)
import WAGS.Properties (frequency)
import WAGS.Properties as P
import WAGS.Run (run2e)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V
  ( startStop :: StartStop
  , slider :: Number
  )

random :: Behavior Number
random = behavior \\e ->
  makeEvent \\k -> subscribe e \\f ->
    Random.random >>= k <<< f

-- pentatonic scale
cp :: Number -> Number
cp n
  | n < 0.142857 = 261.625565
  | n < 0.285714 = 293.664768
  | n < 0.428571 = 349.228231
  | n < 0.571429 = 391.995436
  | n < 0.714286 = 440.000000
  | n < 0.857143 = 523.251131
  | otherwise = 587.329536

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        start = event.startStop.start <|> bang unit
        music :: forall lock. _ -> Event (Array (Node _ lock _))
        music evt' = memoize evt' \\evt -> do
          let
            pitch = map fst evt
            -- to avoid artifacts in the pitch change
            time = map (add 0.01 <<< snd) evt
            e0 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.6, 0.2, 0.1, 0.5, 0.03, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            e1 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.3, 0.1, 0.05, 0.01, 0.005, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            e2 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.15, 0.05, 0.01, 0.005, 0.0005, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            f0 = bangOn <|> frequency <<< cp <$> pitch
          [ fan1 (triangleOsc 0.0 f0) \\ipt _ -> do
              gain_ 2.0
                [ gain 0.0 (P.gain <$> e0)
                    [ bandpass_
                        { frequency: 1000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                , gain 0.0 (P.gain <$> e1)
                    [ bandpass_
                        { frequency: 2000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                , gain 0.0 (P.gain <$> e2)
                    [ highpass_
                        { frequency: 4000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                ]
          ]
      D.div_
        [ D.div_
            [ text_ "tempo"
            , D.input
                ( oneOfMap bang
                    [ D.Xtype := "range"
                    , D.Min := "0"
                    , D.Max := "100"
                    , D.Step := "1"
                    , D.Value := "50"
                    , D.OnInput := cb
                        ( traverse_
                            (valueAsNumber >=> push.slider)
                            <<< (=<<) fromEventTarget
                            <<< target
                        )
                    ]
                )
                blank
            ]
        , D.button
            ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                [ start $> do
                    ctx <- context
                    let
                      myIvl = sampleBy Tuple random
                        $ interval ctx 0.91
                        $ map (calcSlope 0.0 0.42 100.0 1.4)
                        $ event.slider
                    r <- run2e ctx (music myIvl)
                    push.startStop.stop (r *> close ctx)
                , event.startStop.stop <#>
                    (_ *> push.startStop.start unit)
                ]
            )
            [ text $ oneOf
                [ start $> "Turn on"
                , event.startStop.stop $> "Turn off"
                ]
            ]
        ]
  )`,A1=nu(function(t){return qt(function(e){return St(t)(function(r){return function(){var a=Gu();return e(r(a))()}})})}),y1=function(){return d.value}(),k1=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(Ze)return 587.329536;throw new Error("Failed pattern match at WAGS.Example.Docs.Events.Ex2 (line 225, column 1 - line 225, column 23): "+[t.constructor.name])},yh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2>Example 3: Fascinating rhyhtm</h2>

  <p>Wags comes with several different ways to hook into the Web Audio API's sample-accurate timers. In this section, we'll use a Wags <code>interval</code> event to create a sample-accurate ticker. We'll also use a <code>random</code> beahvior to change up our samples.</p>

  <p><code>interval :: AudioContext -> Event Number -> Event Number</code> in wags is similar to <a href=""><code>interval :: Int -> Event Instant</code></a> from the <code>Event</code> library with a few important exceptions.</p>

  <ul>
    <li>The wags interval works in seconds (<code>Number</code>) instead of milliseconds.</li>
    <li>The wags interval needs an audio context to work.</li>
    <li>The wags interval gets its timing from an <code>Event Number</code> instead of a plain old <code>Number</code>. This is necessary to have variable rates.</li>
  </ul>

  <blockquote><code>interval</code> works fine for a stream of events where each event is separated by more than ~100 milliseconds. For anything faster, you'll likely want to use <code>requestAnimationLoop</code> coupled with a local state, as it will be more efficient for older and battery-sensitive devices.</blockquote>

  <p>In the following example, we use <code>interval</code> to control the playback rate of an analogue synth. We'll also use a custom behavior called <code>random</code> to control the pitch.</p>

  <p>One important optimization we make here is the use of the function <code>memoize</code>. Whenever we're dealing with audio-clock timing, we want to limit the number of subscriptions to receive events from the audio clock. Ideally, there is only one subscription that takes a reading of the clock as a single source of truth. Because we are in PureScript-land, events (like everything else), are referrentially transparent, meaning that new ones will get created every time you use them (just like a new <code>2</code> is created every time you type the value <code>2</code>: they don't all refer to one uber-<code>2</code>). To sync all the events to the <i>same</i> source, we use <code>memoize</code>. While this optimization is not necessary, I recommend it: it will make sure the timing is 100% accurate at a very small energy cost (meaning <code>memoize</code> will eat up slightly more power from a phone's battery, but still not much).</p>

  <pre><code>@txt@</code></pre>

  @ex2@

</section>
`}})({reflectType:function(){return"@"}})()()(o)(I()(I()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})(o))(d.value)(y1)({txt:R(or(ue)(b1)),ex2:R(rt(vu()(o)(ur({reflectSymbol:function(){return"slider"}})()()(Iu({reflectSymbol:function(){return"startStop"}})()()()(ur({reflectSymbol:function(){return"start"}})()()(ur({reflectSymbol:function(){return"stop"}})()()(Hn)()()()())()()()())(Hn)()()()())()()()())(d.value)(function(n){return function(a){var u=O(N(c))(a.startStop.start)(Y(S(o))(void 0)),f=function(i){return qi(o)(i)(function(m){var s=p(k)(function(){var ot=Le(ka)(.01);return function(vt){return ot(on(vt))}}())(m),_=p(k)(Ua)(m),v=O(N(c))(_t())(p(k)(function(){var ot=Co()(os);return function(vt){return ot(k1(vt))}}())(_)),D=p(k)(function(ot){return es(function(vt){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:vt}}(ot))})(s),l=p(k)(function(ot){return es(function(vt){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:vt}}(ot))})(s),C=p(k)(function(ot){return es(function(vt){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:vt}}(ot))})(s);return[$a(ds(is)(0)(v))(function(ot){return function(vt){return Ot(it)(2)([Hr(it)(0)(p(k)(Fn()(Rn))(C))([hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:1e3,q:20})([ot])]),Hr(it)(0)(p(k)(Fn()(Rn))(l))([hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:2e3,q:20})([ot])]),Hr(it)(0)(p(k)(Fn()(Rn))(D))([Fc($D(yt(At()(z(z(bt)(TC)()()()({reflectSymbol:function(){return"q"}}))(bD)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:4e3,q:20})([ot])])])}})]})};return Be(o)([Be(o)([or(ue)("tempo"),So(o)(Nr(wt)(g(c))(Y(S(o)))([K(jo)(Eo.value)("range"),K(ef)(Ei.value)("0"),K(tf)(Ci.value)("100"),K(nf)(hi.value)("1"),K(af)(Ti.value)("50"),K(rf)(Si.value)(Qe(function(){var i=nr(c)(Je)(Vf(hr)(Gf)(n.slider)),m=Yn(Ea)(Uc);return function(s){return i(m(Xc(s)))}}()))]))([])]),Tn(o)(Nr(wt)(g(c))(p(k)(function(){var i=K(sr)(pr.value);return function(m){return i(Qe(T(m)))}}()))([tr(k)(On(S(o))(O(N(c))(Y(S(o))(h(c)(void 0)))(p(k)(function(i){return i.value0})(r)))(X(k)(u)(Z(et))))(function(i){return function(){i();var s=oa(ir)(),_=gi(S(o))(nt.create)(A1)(RS(s)(.91)(p(k)(Oa(0)(.42)(100)(1.4))(a.slider))),v=Lc(s)(f(_))(),D=j(tt)(v)(xn(ir)(s));return t(j(tt)(D)(n.startStop.start(void 0)))(),n.startStop.stop(j(tt)(D)(xn(ir)(s)))()}}),tr(k)(a.startStop.stop)(function(i){return j(tt)(i)(j(tt)(t(h(c)(void 0)))(n.startStop.start(void 0)))})]))([ln(ue)(Ve(wt)(g(c))([X(k)(u)("Turn on"),X(k)(a.startStop.stop)("Turn off")]))])])}})))})}}};var C1=function(){return d.value}(),kh=function(){return me({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(o)(Q(c))(d.value)(C1)({})}();var S1=function(){return d.value}(),gh=function(){return me({reflectType:function(){return`<section>
  <h2>Events in Wags</h2>
  <p>Wags follows a consistent pattern: every audio unit accepts an event containing a <code>newtype</code> around a <code>Variant</code> of parameters that can be changed. As a motivating example, let's look at the definition of <code>sinOsc</code>.</p>

  <pre><code>sinOsc
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialSinOsc i
  => i
  -> event C.SinOsc
  -> C.Node outputChannels "" C.G_ event payload</code></pre>

  <p>In that defintion, <code>SinOsc</code> is a <code>newtype</code> that is defined as such:</p>

  <pre><code>newtype SinOsc = SinOsc
  (Variant (frequency :: AudioParameter, onOff :: AudioOnOff))</code></pre>

  <p>Informally, the <code>sinOsc</code> function listens to an event that contains one of several possible instructions, including changing the frequency and turning the oscillator on or off. Let's see a few more examples:</p>

    <table>
    <tr>
      <th>Audio Unit</th>
      <th>Newtype name</th>
      <th>Variant</th>
    </tr>
    <tr>
      <td><code>delay</code></td>
      <td><code>Delay</code></td>
      <td><code>Variant (delayTime :: AudioParameter)</code></td>
    </tr>
    <tr>
      <td><code>highshelf</code></td>
      <td><code>Highshelf</code></td>
      <td><code>Variant (frequency :: AudioParameter, gain :: AudioParameter)</code></td>
    </tr>
    <tr>
      <td><code>playBuf</code></td>
      <td><code>PlayBuf</code></td>
      <td><code>Variant (playbackRate :: AudioParameter, buffer :: BrowserAudioBuffer, duration :: Maybe Number, bufferOffset :: Number, onOff :: AudioOnOff)</code></td>
    </tr>
  </table>

  <p>In practice, you'll never need to use newtypes presented above. The <code>WAGS.Properties</code> module has highly-overloaded smart constructors for all these values. For example, to use an <code>Event Number</code> to set the frequency of a <code>sinOsc</code>, you'd write <code>frequency &lt;$&gt; event</code> instead of <code>SinOsc &lt;&lt;&lt; inj (Proxy :: _ "frequency") &lt;&lt;&lt; AudioParameter &lt;&lt;&lt; { o: 0.0, t: _linearRamp, n: _ } &lt;$&gt; event</code>. Of course, you <i>can</i> write the longer format, but life is short and keystrokes are precious! Below is a table showing the varoius smart constructors available and the units to which they apply.</p>


    <table>
    <tr>
      <th>Constructor</th>
      <th>Type</th>
      <th>Receptive Audio Units</th>
    </tr>
    <tr>
      <td><code>bufferOffset</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>playBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>delayTime</code></td>
      <td><code>AudioParameter</code></td>
      <td><ul><li><code>delay</code></li></ul></td>
    </tr>
    <tr>
      <td><code>duration</code></td>
      <td><code>Maybe Number</code></td>
      <td><ul><li><code>playBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>frequency</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>allpass</code></li>
          <li><code>bandpass</code></li>
          <li><code>highpass</code></li>
          <li><code>highshelf</code></li>
          <li><code>lowpass</code></li>
          <li><code>lowshelf</code></li>
          <li><code>notch</code></li>
          <li><code>peaking</code></li>
          <li><code>periodicOsc</code></li>
          <li><code>sawtoothOsc</code></li>
          <li><code>sineOsc</code></li>
          <li><code>squareOsc</code></li>
          <li><code>triangleOsc</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>gain</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>gain</code></li>
          <li><code>highshelf</code></li>
          <li><code>lowshelf</code></li>
          <li><code>peaking</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>loopEnd</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>loopBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>loopStart</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>loopBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>offset</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>constant</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>onOff</code></td>
      <td><code>AudioOnOff</code></td>
      <td>
        <ul>
          <li><code>constant</code></li>
          <li><code>loopBuf</code></li>
          <li><code>periodicOsc</code></li>
          <li><code>playBuf</code></li>
          <li><code>sawtoothOsc</code></li>
          <li><code>sineOsc</code></li>
          <li><code>squareOsc</code></li>
          <li><code>triangleOsc</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>playbackRate</code></td>
      <td><code>AudioParameter</code></td>
      <td><ul><li><code>loopBuf</code></li><li>playBuf</li></ul></td>
    </tr>
    <tr>
      <td><code>q</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>allpass</code></li>
          <li><code>bandpass</code></li>
          <li><code>highpass</code></li>
          <li><code>lowpass</code></li>
          <li><code>notch</code></li>
          <li><code>peaking</code></li>
        </ul>
      </td>
    </tr>
  </table>

  <p>You can use this smart-constructor pattern to transform any <code>Event</code> into something that Wags can consume. For example:</p>

  <ul>
    <li>If you have an <code>Event Number</code> called <code>myFreq</code> and you'd like it to control the frequency of a band-pass filter, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq)</code>.</li>
    <li>If you have an <code>Event Number</code> called <code>myQ</code> and you'd like it to control the Q value of the same bandpass, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq &lt;|&gt; q &lt;$&gt; myQ)</code> <i>or</i> <code>bandpass 440.0 $ oneOf [frequency &lt;$&gt; myFreq, q &lt;$&gt; myQ]</code>.</li>
    <li>If you'd like <code>myFreq</code> <i>only</i> to have an effect when it's over <code>1000.0</code>, you can write <code>bandpass 440.0 (frequency &lt;$&gt; filter (_ > 1000.0) myFreq &lt;|&gt; q &lt;$&gt; myQ)</code>.</li>
  </ul>

  <p>None of these transformations are unique to Wags:</p>
  <ul>
    <li>Because <code>Event</code> implements <a href=""><code>Functor</code></a>, you can use <code>map</code> (aka <code>&lt;$&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Alt</code></a>, you can use <code>alt</code> (aka <code>&lt;|&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Plus</code></a>, you can use <code>empty</code> for an event that emits nothing as well as <code>oneOf</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>Filterable</code></a>, you can use <code>filter</code>, <code>filterMap</code>, <code>partition</code>, <code>partitionMap</code>, and <code>compact</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>IsEvent</code></a>, you can use <code>bang</code> to emit something right away, <code>sampleOn</code> to sample one event's most recent value based on another event, and all of the other functions rolled into <a href=""><code>IsEvent</code></a>.</li>
  </ul>

  <p>
    This gets to one of the core design principles of Wags. Idiomatic Wags projects use functional reactive programming as a way to "steer" web audio, and Wags aims to be a minimal viable framework to shepherd events to their web-audio destinations.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(o)(Q(c))(d.value)(S1)({})}();var T1=function(){return d.value}(),Ch=function(){return me({reflectType:function(){return`<section>

  <h2>Events, a primer</h2>

  <p>The <code>Event</code> and <code>Behavior</code> types in PureScript are defined as such:</p>

  <pre><code>newtype Event a =
    Event ((a -> Effect Unit) -> Effect (Effect Unit))

newtype ABehavior event a =
  ABehavior (forall b. event (a -> b) -> event b)
type Behavior = ABehavior Event
</code></pre>

  <p>Let's unpack what the contract of both types are saying.</p>

  <h3>Event</h3>

  <p>An event takes a pusher of type <code>a -> Effect Unit</code> to which you can push values of type <code>a</code>. What are the values? Whatever you want! It could be a mouse click, a slider's input, an animation loop thunk, whatever. The event returns a nested <code>Effect</code> - the outer one is triggered on event subscription and the inner one is triggered on event unsubscription. In the case of a click listener, for example, the outer effect will likely call <code>addEventListener</code> and the inner will likely call <code>removeEventListener</code>.</p>

  <p>
    When using Wags, you have to get your events from somewhere. At a minimum, you'll consume a browser interaction like a click or swipe that turns on the audio. In fact, without some form of human interaction, most browsers will block the Web Audio API from turning on.
  </p>
  <p>
    <code>Events</code> are often produced within a web framework like <a href="https://github.com/mikesol/purescript-deku">Deku</a>, Halogen or React. They don't have to be, though - you can create and consume your own events.
  </p>

  <h3>Behavior</h3>

  <p>
    The <code>Behavior</code> type takes an event that needs to be "unlocked" (meaning in the form of <code>a -> b</code>, so an <code>a</code> is needed to unlock a <code>b</code>) and unlocks it with an <code>a</code>. Behaviors don't need to produce their <code>a</code> immediately. In fact, they don't need to produce it at all: it's entirely possible to create <code>Behavior (const empty)</code> that "mutes" the event. This resembles the physical world: when we want to observe a behavior, like the weather outside or the axial rotation of the Earth, there is a time-cost to observing anything that ranges from instantaneous to infinite.
  </p>

  <p>
    In Wags, we usually want to observe the behavior of things like a mouse's position, an audio buffer's content or a random number generator.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(o)(Q(c))(d.value)(T1)({})}();var F1=function(){return d.value}(),Eh=function(t){return function(e){return function(r){return function(n){var a=function(f){return wa(N(c))(S(o))(n)(j(tt)(e(f))(vn))},u=Pa(t)(r);return me({reflectType:function(){return`<div>
  <h1>Events</h1>

  <h3>Clicks, wiggles and loops, oh my!</h3>
  <p>
    The true magic of web audio lies in its ability to harness the rich interactivity built into the browser. We can use mouse clicks, finger swipes and animation loops to create beautiful audio landscapes. But how can we tame the complexity of all these events in an expressive, declarative, functional manner? Enter <code>Event</code>, the abstraction that allows us to build rich reactive works using Wags.
  </p>

  @primer@
  @inWags@
  @flavors@
  @ex0@
  @ex1@
  @ex2@

  <h2>Next steps</h2>
  <p>In this section, saw how to build rich audio applications using the <code>Event</code> and <code>Behavior</code> types. We also covered the three most common patterns you'll see when working with events: events that need to happen <i>now</i>, events that come from user interaction, and timed events. In the next section, we'll look at different ways to specify <a @next@ style="cursor:pointer;">the numeric parameters being sent as events</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(o)(I()(I()(I()(I()(I()(_n()(I()(Q(c))({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}})(o))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"inWags"}})({reflectSymbol:function(){return"inWags"}})(o))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}})(o))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})(o))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(o))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})(o))(d.value)(F1)({next:a(h_.value),primer:R(Ch),inWags:R(gh),flavors:R(kh),ex0:R(dh(u)(e)(n)),ex1:R(Ah(u)(e)(n)),ex2:R(yh(u)(e)(n))})}}}};var M1=function(){return d.value}(),Sh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0 do
      let ooo n = bang $ onOff $ dt (add n) apOn
      [ playBuf tink0 (ooo 0.1)
      , playBuf tink1 (ooo 0.2)
      , playBuf tink2 (ooo 0.9)
      , playBuf tink3 (ooo 1.8)
      ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(I()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(M1)({ai0:R(rt(ht(r)(t)(function(n){return ko(Ln)(zt(bi)(zt(bi)(zt(bi)(p(Ic)(function(a){return function(u){return function(f){return function(i){return{tink0:a,tink1:u,tink2:f,tink3:i}}}}})(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return mt(n)([Ot(it)(1)(function(){var u=function(f){return Y(S(o))(Bf()(Mf)(us()(Le(ka)(f))(l_)))};return[Qn(za)(a.tink0)(u(.1)),Qn(za)(a.tink1)(u(.2)),Qn(za)(a.tink2)(u(.9)),Qn(za)(a.tink3)(u(1.8))]}())])}})))})}}};var P1=function(){return d.value}(),hh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0
      $ do
          let
            ooo n = bang $ onOff $ dt (add n) apOn
            mtk i =
              case i \`mod\` 4 of
                0 -> tink0
                1 -> tink1
                2 -> tink2
                _ -> tink3
          0 .. 100 &lt;#&gt;
            \\i' -> do
              let i = toNumber i'
              playBuf (mtk i')
                (ooo (0.3 + 0.3 * (i * (1.005 \`pow\` i))))</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(I()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(P1)({ai0:R(rt(ht(r)(t)(function(n){return ko(Ln)(zt(bi)(zt(bi)(zt(bi)(p(Ic)(function(a){return function(u){return function(f){return function(i){return{tink0:a,tink1:u,tink2:f,tink3:i}}}}})(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return mt(n)([Ot(it)(1)(function(){var u=function(i){return Y(S(o))(Bf()(Mf)(us()(Le(ka)(i))(l_)))},f=function(i){var m=iu($o)(i)(4);return m===0?a.tink0:m===1?a.tink1:m===2?a.tink2:a.tink3};return tr(xe)(dn(0)(100))(function(i){var m=qe(i);return Qn(za)(f(i))(u(.3+.3*(m*Vi(1.005)(m))))})}())])}})))})}}};var I1=function(){return d.value}(),Th=function(t){return function(e){return function(r){return me({reflectType:function(){return`<div>
  <pre><code>run2_
  [ fan1 (loopBuf buf bangOn)
      \\b _ -> gain_ 0.8
        [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
        , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
        , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
        , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
        , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(I()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(I1)({ai0:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([$a(le(ge)(a)(_t()))(function(u){return function(f){return Ot(it)(.8)([hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:400,q:1})([u]),hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:880,q:5})([u]),hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:1200,q:10})([u]),hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:2e3,q:20})([u]),hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:3e3,q:30})([u])])}})])}})))})}}};var N1=function(){return d.value}(),xh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
      \\b _ -> gain_ 0.8
        $ 0 .. 40 &lt;#&gt; lcmap toNumber
            \\i -> bandpass_
              { frequency: 200.0 + i * 150.0, q: 30.0 }
              [ b ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(I()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(N1)({ai0:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([$a(le(ge)(a)(_t()))(function(u){return function(f){return Ot(it)(.8)(tr(xe)(dn(0)(40))(oo(Ta)(qe)(function(i){return hn(mn(yt(At()(z(z(bt)(Sn)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:200+i*150,q:30})([u])})))}})])}})))})}}};var W1=function(){return d.value}(),Fh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(I()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(W1)({ai0:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return mt(n)([Ka(function(u){return Ot(it)(1)([Qn(za)(a)(_t()),yo(en)(.1)([Ot(it)(.6)([u])])])})])}})))})}}};var B1=function(){return d.value}(),U1=function(t){return function(e){return Y(t)(Fn(e)(Rn)({p:[1,1,0],o:0,d:10}))}},q1=function(t){return function(e){return Y(t)(Fn(e)(Rn)({p:[1,1,0],o:0,d:8}))}},Qc=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return yo(t)(n)([Ot(e)(a)([Fc(r)(u)(f)])])}}}}}}},$h=function(t){return function(e){return function(r){return me({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(I()(I()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(B1)({txt:R(or(ue)(`dgh d g h i =
  delay_ d [gain_ g [highpass_ h i]]

fade0 = bang
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 8.0 }

fade1 = bang
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 10.0 }

scene buf = run2_
  [ fan1 (playBuf buf bangOn) \\b _ -> mix $ fix
      \\g0 -> gain_ 1.0
        [ b
        , dgh 0.15 0.7 1500.0
            [ fix
                \\g1 -> gain 1.0 fade1
                  [ dgh 0.4 0.5 2500.0
                      [ g0, g1 ]
                  ]
            ]
        , dgh 0.29 0.85 2000.0
            [ fix
                \\g1 -> gain_ 1.0
                  [ dgh 0.6 0.6 3500.0
                      [ g0
                      , ( fix
                            \\g2 -> gain 1.0 fade0
                              [ dgh 0.75 0.6 4000.0
                                  [ g1, g2 ]
                              , dgh 0.75 0.55 3000.0 [ b ]
                              ]
                        )
                      ]
                  ]
            ]
        ]
  ]`)),ai0:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return mt(n)([$a(Qn(za)(a)(_t()))(function(u){return function(f){return Ka(function(i){return Ot(it)(1)([u,Qc(en)(it)(Qa)(.15)(.7)(1500)([Ka(function(m){return Hr(it)(1)(U1(S(o))())([Qc(en)(it)(Qa)(.4)(.5)(2500)([i,m])])})]),Qc(en)(it)(Qa)(.29)(.85)(2e3)([Ka(function(m){return Ot(it)(1)([Qc(en)(it)(Qa)(.6)(.6)(3500)([i,Ka(function(s){return Hr(it)(1)(q1(S(o))())([Qc(en)(it)(Qa)(.75)(.6)(4e3)([m,s]),Qc(en)(it)(Qa)(.75)(.55)(3e3)([u])])})])])})])])})}})])}})))})}}};var z1=function(){return d.value}(),Mh=function(t){return function(e){return function(r){return function(n){var a=function(u){return wa(N(c))(S(o))(n)(j(tt)(e(u))(vn))};return me({reflectType:function(){return`<section>
  <p>
    In the <a @hwLink@ style="cursor:pointer;">hello world</a> section, we saw how to create and wire up two audio nodes: a <code>sinOsc</code>, or a sine-wave oscillator, is hooked up to a <code>gain</code> node. For some cases, feeding one audio node to another all the way up to a loudspeaker will be all you need. However, in most cases, you'll need to exploit three additional relationships:</p>
    <ul>
      <li><span style="font-weight:800px;">Many to one</span>, where many audio units pass through one.</li>
      <li><span style="font-weight:800px;">One to many</span>, where a single audio unit passes through many different ones.</li>
      <li><span style="font-weight:800px;">Feedback</span>, where an audio unit is an input to itself.</li>
    </ul>
    <p>This section will show how wags handles all three cases:</p>
    <ul>
      <li><span style="font-weight:800px;"><code>Array</code></span> is the data structure we'll use to send many audio units into one.</li>
      <li><span style="font-weight:800px;"><code>fan</code></span> is a function that we'll use to "fan" one audio node out to many.</li>
      <li><span style="font-weight:800px;"><code>fix</code></span> is the function we'll use to make an audio unit an input into itself.</li>
    </ul>
    <h2>The setup</h2>
    <p>
      To illustrate how <code>Array</code>, <code>fan</code> and <code>fix</code> work, we're going to use a few new audio units.
    </p>
      <ul>
        <li><code>delay</code>: A delay node</li>
        <li><code>bandpass</code>: A bandpass filter, meaning a filter that lets a single frequency band pass through.</li>
        <li><code>playBuf</code> and <code>loopBuf</code>: Playback or looping playback of a buffer. We'll use some MP3 files from freesound.org.</li>
      </ul>
  </section>`}})({reflectType:function(){return"@"}})()()(o)(_n()(Q(c))({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}})(o))(d.value)(z1)({hwLink:a(Nf.value)})}}}};var J1=function(){return d.value}(),wh=function(t){return function(e){return function(r){return function(n){var a=function(f){return wa(N(c))(S(o))(n)(j(tt)(e(f))(vn))},u=Pa(t)(r);return me({reflectType:function(){return`<div>
  <h1>Array, fan, and fix</h1>

  <h3>The anatomy of a Wags graph</h3>

  @intro@

  <h2>Arrays</h2>

  <p>To send several audio units through one, we use an <code>Array</code>.</p>

  @code0@

  <p>PureScript <code>Array</code>-s are extremely flexible and efficient, so go to town! For example, you can <code>map</code> (aka <code>&lt;#&gt;</code> when flipped) over a range of integers to create audio units, like in the example below.</p>

  @code1@

  <h2>Fan</h2>

  <p><span style="font-weight:800;">Fan</span> takes a vector of audio signals and fans it out to multiple processing chains. If you have a single signal, you can use <code>fan1</code>. For example, if you have a looping buffer and you'd like to filter it through a bank of different filters, you can do this via fan. Fan takes two arguments:</p>

  <ul>
    <li>A vector of nodes to fan out (or a single node in the case of <code>fan1</code>).</li>
    <li>A function that accepts a reference to this/these node(s) and returns a new node that may or may not contain the input.</li>
  </ul>

  <p>Let's see an example below that fans one <code>playBuf</code> to five bandpass filters.</p>

  @code2@

  <p>Just for kicks, let's jack it up to forty bandpass filters.</p>

  @code3@

  <h2>Fix</h2>

  <p><span style="font-weight:800;">Fix</span> is a fixed point operator. It accepts itself as an argument and returns... itself \u{1F92F}. You can use <code>fix</code> to create feedback loops!</p>

  @code4@

  <blockquote>If you don't have some sort of delay line in your processing chain, either via the Web-Audio-provided delay line or a custom delay node, Web Audio will raise a runtime error. Wags doesn't check for this, so make sure you test your audio to guarantee that it's feedback-explosion-free!</blockquote>

  <p>Nothing stops you from nesting <code>fix</code>-s to create a mega-feedback loop!</p>

  <blockquote>In the example below, I've added a couple fades to make sure the experience isn't too unpleasant. We'll talk more about fades in the events section \u{1F3B8}</blockquote>

  @code5@

  <h2>Next steps</h2>
  <p>In this section, saw how to combine together audio nodes with arrays, fan one audio node to many processing chains via <code>fan</code>, and how to create a fixed point, aka feedback, for a node via <code>fix</code>. In the next section, we'll ramp up on all of the yummy <a @next@ style="cursor:pointer;">audio nodes you can use</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(o)(I()(I()(I()(I()(I()(I()(I()(_n()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}})(o))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}})(o))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}})(o))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}})(o))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}})(o))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}})(o))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}})(o))(d.value)(J1)({intro:R(Mh(t)(e)(r)(n)),next:a(E_.value),code0:R(Sh(u)(e)(n)),code1:R(hh(u)(e)(n)),code2:R(Th(u)(e)(n)),code3:R(xh(u)(e)(n)),code4:R(Fh(u)(e)(n)),code5:R($h(u)(e)(n))})}}}};var Ph=function(t){return function(e){return function(r){return new U(J(t)("code")(e)(new H(r)))}}},Eb=function(t){return Ph(t)($(g(t.Monad0().Applicative0())))};var Oh=function(t){return function(e){return function(r){return new U(J(t)("pre")(e)(new H(r)))}}},Sb=function(t){return Oh(t)($(g(t.Monad0().Applicative0())))};var K1=function(){return d.value}(),Ih=function(t){return function(e){return function(r){return function(n){var a=j(tt)(e(S_.value))(vn),u=Pa(t)(r);return me({reflectType:function(){return`<div>
  <h1>Hello world</h1>

  <h3>Wagging at 440Hz</h3>

  <p>Here's a "hello world" in Wags. In this and all the following sections, we'll start with a full example, and we'll pick it apart afterwards.</p>

  @code@
  @result@

  <h2>The <code>run</code> functions</h2>

  <p>The <code>run</code> family of functions run our audio and produces an unsubscribe function that we use to stop the audio. In this case, <code>run2_</code> does three extra things:
  <ul>
    <li>Wires up our session for two-channel audio. If the sources are mono, it will automatically scale them up to stereo.</li>
    <li>Automatically handles creation and destruction of audio contexts.</li>
    <li>Takes care of the subscription to the rendering engine.</li>
  </ul></p>

  <p>The <code>push</code> function comes from the <a href="https://github.com/mikesol/purescript-deku"><code>purescript-deku</code></a> framework and is used to register an unsubscribe effect. When we trigger the effect it in the <code>Just</code> branch of our pattern match, the audio turns off.</p>

  <h2>Audio units</h2>

  <p>The sound you hear when you play the example above is created with the statement <code>gain_ 0.15 [ sinOsc 440.0 bangOn ]</code>. The first function, <code>gain_</code>, creates a gain node with a volume of <code>0.15</code>. In WebAudio, gain ranges from <code>0.0</code> to <code>1.0</code> and can be converted to decibels using the following equation:</p>

  <pre><code>decibels = 20 * log10( gain );</code></pre>

  <p>In our case, a gain of <code>0.15</code> is roughly <code>-16.5 dB</code>.</p>

  <p>Our sine wave oscillator is set to a frequency of <code>440Hz</code>. That means that your loudspeaker or headphones will vibrate back and forth in sinusoidal motion 440 times per second, which most folks perceive as the <a href="https://en.wikipedia.org/wiki/A440_(pitch_standard)">note A</a>. And we turn on the oscillator with <code>bangOn</code>, as the default is off for <i>all</i> sound generators in Wags. This is a design decision to help preserve the hearing of those that work frequently with audio.</p>

  <h2>Next steps</h2>
  <p>Now that we have our setup running, let's explore the anatomy of a Wags graph. Irrespective of the nodes comprising the graph, there are three basic concepts you need to be familiar with before you start diving into audio units: <a @next@ style="cursor:pointer;">array, fan, and fix</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(o)(I()(_n()(I()(Q(c))({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}})(o))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})(o))(d.value)(K1)({code:R(Sb(o)([Eb(o)([or(ue)(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])])),result:R(rt(ht(n)(u)(function(f){return h(ba)(void 0)})(function(f){return function(i){return mt(f)([Ot(it)(.15)([Rf(Xi)(440)(_t())])])}}))),next:wa(N(c))(S(o))(n)(a)})}}}};var Rh=ac;var Nh=function(){return function(t){return t}};var Lh=function(){return function(t){return t}};var hb=function(){function t(){}return t.value=new t,t}();var Wh={attr:function(t){return function(e){return b({key:"height",value:L(e)})}}};var Tb=function(){function t(){}return t.value=new t,t}();var Gh={attr:function(t){return function(e){return b({key:"width",value:L(e)})}}};var xb=function(t){return function(e){return function(r){return new U(J(t)("canvas")(e)(new H(r)))}}};var Fb=function(){function t(){}return t.value=new t,t}(),$b={attr:function(t){return function(e){return b({key:"@self@",value:ct(e)})}}};function qs(t){return function(){return t.getContext("2d")}}function N_(t){return function(e){return function(){t.fillStyle=e}}}function Hs(t){return function(){t.beginPath()}}function zs(t){return function(){t.fill()}}function Mb(t){return function(e){return function(){t.arc(e.x,e.y,e.radius,e.start,e.end,e.useCounterClockwise)}}}function Vs(t){return function(e){return function(){t.fillRect(e.x,e.y,e.width,e.height)}}}var vL=function(){return 2*Ji}(),Kc=function(t){return{o:t.value0+.04,n:t.value1,t:Uo}};var DL=function(){return d.value}(),Yc=function(t){return function(e){return function(r){return function(n){return Y(t)(Co(e)(Rn)({p:[r,n],o:0,d:16}))}}}},dL=function(t){return function(e){return Y(t)(Fn(e)(Rn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},bL=function(t){return function(e){return Y(t)(Fn(e)(Rn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}};var Js=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return function(m){return function(s){return y_(t)(n)(a)([Hr(e)(u)(f)([GD(r)(i)(m)(s)])])}}}}}}}}}},Bh=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return function(m){return function(s){return y_(t)(n)(a)([Hr(e)(u)(f)([WD(r)(i)(m)(s)])])}}}}}}}}}},AL=function(t){return function(e){return function(r){return function(n){return Y(t)(qc(e)(Rn)({p:[r,n],o:0,d:16}))}}}},Uh=400,wb=qe(Uh),yL=function(){return Xt(ja)(Uh)+"px"}(),qh=600,Pb=qe(qh),kL=function(){return Xt(ja)(qh)+"px"}(),gL={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},Hh=function(t){return function(e){return function(r){return me({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(o)(I()(Q(c))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(o))(d.value)(DL)({ex1:R(rt(vu()(o)(ur({reflectSymbol:function(){return"canvas"}})()()(ur({reflectSymbol:function(){return"slider"}})()()(Iu({reflectSymbol:function(){return"startStop"}})()()()(ur({reflectSymbol:function(){return"loading"}})()()(ur({reflectSymbol:function(){return"start"}})()()(ur({reflectSymbol:function(){return"stop"}})()()(Hn)()()()())()()()())()()()())(Hn)()()()())()()()())()()()())(d.value)(function(n){return function(a){var u=O(N(c))(Y(S(o))(void 0))(a.startStop.start),f=function(i){return function(m){return function(s){var _=p(k)(function(v){return new nt(v.acTime,v.value)})(zc(i)(a.slider));return[Ds(vs(yt(At()(z(z(bt)(ms)()()()({reflectSymbol:function(){return"fftSize"}}))(ss)()()()({reflectSymbol:function(){return"cb"}})))(dt()())))({cb:function(v){return function(){return Mn(new B(v))(s)(),Mn(V.value)(s)}},fftSize:Zp.value})(h(lr)($a(Qn(za)(m)(O(N(c))(_t())(p(k)(function(){var v=ia()(Lr),D=Xn(gn)(Oa(0)(.96)(100)(1.04));return function(l){return v(Kc(D(l)))}}())(_))))(function(v){return function(D){return Ka(function(l){return Ot(it)(1)([v,y_(MD(yt(At()(z(z(bt)(xC)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(AD)()()()({reflectSymbol:function(){return"delayTime"}})))(dt()())))({maxDelayTime:2.5,delayTime:1})(p(k)(function(){var C=qc()(Lr),ot=Xn(gn)(Oa(0)(.5)(100)(2.45));return function(vt){return C(Kc(ot(vt)))}}())(_))([Hr(it)(.4)(p(k)(function(){var C=Fn()(Lr),ot=Xn(gn)(Oa(0)(.6)(100)(.9));return function(vt){return C(Kc(ot(vt)))}}())(_))([v])]),Js(en)(it)(Qa)(.15)($(g(c)))(.7)($(g(c)))(1500)(Yc(S(o))()(1500)(3e3))([Ka(function(C){return Hr(it)(1)(dL(S(o))())([Js(en)(it)(Qa)(.4)($(g(c)))(.5)($(g(c)))(3e3)(Yc(S(o))()(3e3)(100))([l,C])])})]),Js(en)(it)(Qa)(.29)(p(k)(function(){var C=qc()(Lr),ot=Xn(gn)(Oa(0)(.1)(100)(.4));return function(vt){return C(Kc(ot(vt)))}}())(_))(.85)($(g(c)))(2e3)(Yc(S(o))()(2e3)(5e3))([Ka(function(C){return Ot(it)(1)([Js(en)(it)(Qa)(.6)(p(k)(function(){var ot=qc()(Lr),vt=Xn(gn)(Oa(0)(.8)(100)(.3));return function(Gt){return ot(Kc(vt(Gt)))}}())(_))(.6)($(g(c)))(3500)(Yc(S(o))()(3500)(100))([l,Ka(function(ot){return Hr(it)(1)(bL(S(o))())([Bh(en)(it)(wD)(.75)(p(k)(function(){var vt=qc()(Lr),Gt=Xn(gn)(Oa(0)(.9)(100)(.1));return function(re){return vt(Kc(Gt(re)))}}())(_))(.6)($(g(c)))(4e3)(Yc(S(o))()(4e3)(200))([C,ot]),Bh(en)(it)(wD)(.75)(AL(S(o))()(.75)(.2))(.55)($(g(c)))(200)(Yc(S(o))()(200)(4e3))([v])])})])])})])])})}})))]}}};return Be(o)([xb(o)(O(N(c))(Nr(wt)(g(c))(Y(S(o)))([K(Gh)(Tb.value)(kL),K(Wh)(hb.value)(yL),K(rk)(Jt.value)("width: 100%;"),K($b)(Fb.value)(function(){var i=nr(c)(Je)(function(m){return function(){var _=qs(m)();return N_(_)("black")(),Vs(_)({width:Pb,height:wb,x:0,y:0})(),void 0}});return function(m){return i(ab(m))}}())]))(p(k)(function(i){return K($b)(Fb.value)(function(){var m=nr(c)(Je)(function(s){return function(){var v=qs(s)();return N_(v)("black")(),Vs(v)({width:Pb,height:wb,x:0,y:0})(),N_(v)("rgba(255,255,255,0.2)")(),ll(i)(function(D){return function(){return Hs(v)(),Mb(v)({end:vL,radius:D.value1*40,start:0,x:D.value0.x*Pb,y:D.value0.y*wb,useCounterClockwise:!1})(),zs(v)()}})()}});return function(s){return m(ab(s))}}())})(a.canvas)))([]),So(o)(Nr(wt)(g(c))(Y(S(o)))([K(jo)(Eo.value)("range"),K(ef)(Ei.value)("0"),K(tf)(Ci.value)("100"),K(nf)(hi.value)("1"),K(af)(Ti.value)("50"),K(ek)(Jt.value)("width: 100%;"),K(rf)(Si.value)(Qe(function(){var i=nr(c)(Je)(Vf(hr)(Gf)(n.slider)),m=Yn(Ea)(Uc);return function(s){return i(m(Xc(s)))}}()))]))([]),Tn(o)(Ve(wt)(g(c))([Y(S(o))(K(oc)(Jt.value)("width:100%; padding:1.0rem;")),Nr(wt)(g(c))(p(k)(function(){var i=K(sr)(pr.value);return function(m){return i(Qe(T(m)))}}()))([X(k)(a.startStop.loading)(h(c)(void 0)),tr(k)(a.startStop.stop)(function(i){return j(tt)(i)(j(tt)(t(h(c)(void 0)))(n.startStop.start(void 0)))}),tr(k)(On(S(o))(O(N(c))(Y(S(o))(h(c)(void 0)))(p(k)(function(i){return i.value0})(r)))(X(k)(u)(Z(et))))(function(i){return function(){i(),n.startStop.loading(void 0)();var s=_r(V.value)(),_=go(q(Rr)(oa(Ir))(function(v){return q(Rr)(zu(Ir)(v))(function(D){return q(Rr)(p(di)(Lh())(SE(Ln)(Rh)(Ct(v))(Nh()(gL))))(function(l){return q(Rr)(mr(Ir)(Hl(0)(5e4)))(function(C){var ot=vc(Lv(Ca(Mk(l.pluck0))(pc(cv(lv()(l))))))({newSeed:_c(C),size:4});return mr(Ir)(function(){var Gt=ra(ei)(c)(function(lt){return function(){var ke=Gu(),Bt=Gu();return{x:ke,y:Bt}}})(dn(0)(127))(),re=mt(v)(f(v)(ot)(s))(),he=St(Ki)(function(lt){return function(){var ke=wr(s)();return ea(c)(Je)(ke)(function(Bt){return function(){var Su=F_(Bt)(),Vu=p(x)(function(){var xi=kl(Gt),au=p(xe)(function(Dn){return function(Xo){return Xo/255}(Dn)});return function(Dn){return xi(au(Dn))}}())(xs(Ts)(Su))();return n.canvas(Vu)(),void 0}})()}})(),gr=j(tt)(j(tt)(j(tt)(re)(D))(xn(ir)(v)))(he);return n.startStop.stop(gr)(),gr})})})})}))();return t(function(){return n.startStop.start(void 0)(),zo(Ai(_))()})(),void 0}})])]))([ln(ue)(Ve(wt)(g(c))([p(k)(T("Turn off"))(a.startStop.stop),p(k)(T("Turn on"))(u),p(k)(T("Loading..."))(a.startStop.loading)]))])])}})))})}}};var EL=function(){return d.value}(),zh=function(t){return function(e){return function(r){return function(n){var a=Pa(t)(r);return $t({reflectType:function(){return`<div>
  <h1>Wags</h1>

  <h3>A web-audio framework written in PureScript</h3>

  <p>Hi! You've found <a href="https://github.com/mikesol/purescript-wags">Wags</a>.</p>

  <p>Wags is a web-audio framework designed for interactive media and games. Events like mouse clicks, MIDI notes and tweening frames are streamed to an audio rendering engine and, in response to these events, sound happens.</p>

  <h2>Why?</h2>

  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API">Web Audio API</a> is an amazing piece of technology. It is clear, concise, straightforward and ergonomic. So why build a framework on top of it?</p>

  <p>As audio projects become more and more ambitious, a need emerges for powerful abstractions to handle browser events and scheduling. Wags tackles this problem through a small set of <a href="https://en.wikipedia.org/wiki/Functional_reactive_programming">FRP</a>-based abstractions. In doing so, it aims to be concise, expressive, and as fast as manually-optimized hand-written JavaScript.</p>

  <h2>How does it sound?</h2>

  <p>Here's a small example in Wags that, when you turn it on, emits a single sound and then uses feedback loops to create long tail. You can use the slider to change the properties of the tail in real time.</p>

  ~ex~

  <p>By the end of this documentation, you'll know all of the concepts you need to create interactive audio like the example above.</p>

  <p>If you'd like to use this documentation as a springboard for your own work, it can be found <a href="https://github.com/mikesol/purescript-wags/tree/main/examples/docs">here</a>.</p>

  <p>And now, without further ado, let's write a small <a ~next~ style="cursor:pointer;">hello world \xE0 la wags</a>!</p>
</div>`}})()()(I()(_n()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}})(o))(o)(EL)({next:wa(N(c))(S(o))(n)(j(tt)(e(Nf.value))(vn)),ex:R(Hh(a)(e)(n))})}}}};var hL=function(){return d.value}(),Vh=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<div>
  <h1>Merge and split</h1>

  <h3>Inputs and outputs abound!</h3>
  <p>
    Web audio allows you to merge and split arbitrary audio. This is essential when you're working with complex audio setups like 5.1 surround sound or novel headphones used in some gaming setups. Wags allows you to both split and merge arbitrary signals using Web Audio's native merger and splitter nodes.
  </p>

  <h2>Merging</h2>

  <p>Merging audio in wags looks like any node that takes multiple inputs, but instead of accepting something of type <code>AudioInput</code>, it accepts a <i>vector of audio inputs</i>.</p>

  <h2>Splitting</h2>

  <p>Splitting is the inverse operation of merging: it takes a single audio node and splits it into its separate channels. In doing so, it resembles <code>fan</code>, but instead of fanning the audio, it splits it into mono-channel audio.</p>

  <h2>Next steps</h2>
  <p>In this section, saw how to merge and split audio. In the next section, we'll look at how to work with <a ~next~ style="cursor:pointer;">custom audio worklets</a>.</p>
</div>`}})()()(_n()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))(o)(hL)({next:Y(S(o))(K(sr)(pr.value)(Qe(T(j(tt)(e(As.value))(vn)))))})}}}};var xL=function(){return d.value}(),Jh=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2>Cancel</h2>
  <p>The <code>AudioCancel</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/cancelScheduledValues"><code>cancelScheduledValues</code></a> function and cancels whatever effects you programmed in the future. In the example below, we execute the following sequence:</p>
  <ol>
    <li>Play an audio file</li>
    <li>Send an event at 1.0 seconds to schedule an evenlope to modulate the audio rate starting at 1.5 seconds.</li>
    <li>Cancel the envelope at 3.0 seconds, but schedule the cancelation to take effect at 4.0 seconds.</li>
  </ol>
  <pre><code>~txt~</code></pre>
  ~cancel~
  </section>
`}})()()(I()(I()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})(o))(o)(xL)({txt:R(or(ue)(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          ( oneOf
              [ bangOn
              , delay 1000
                  $ bang
                  $ playbackRate
                  $ AudioEnvelope
                      { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                      , o: 1.5
                      , d: 30.0
                      }
              , delay 3000 (bang (playbackRate (AudioCancel { o: 3.5 })))
              ]
          )
      ]
  ]`)),cancel:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Ot(it)(1)([le(ge)(a)(Ve(wt)(g(c))([_t(),Ou(1e3)(Ft(c)(ia()(Rn)({p:Zn(Zo)(X(xe)(dn(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),Ou(3e3)(Ft(c)(ia()(mC)({o:3.5})))]))])])}})))})}}};var $L=function(){return d.value}(),jh=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(I()(I()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})(o))(o)($L)({txt:R(or(ue)(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          ( oneOf
              [ bangOn
              , delay 1000
                  $ bang
                  $ playbackRate
                  $ AudioEnvelope
                      { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                      , o: 1.5
                      , d: 30.0
                      }
              ]
          )
      ]
  ]`)),envelope:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Ot(it)(1)([le(ge)(a)(Ve(wt)(g(c))([_t(),Ou(1e3)(Ft(c)(ia()(Rn)({p:Zn(Zo)(X(xe)(dn(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}})))})}}};var wL=function(){return d.value}(),Xh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2>AudioNumeric</h2>
  <p><code>AudioNumeric</code> encompasses the following three functions from the Web Audio API:</p>

  <ul>
    <li><code>linearRampToValueAtTime</code> via the <code>_linear</code> transition.</li>
    <li><code>exponentialRampToValueAtTime</code> via the <code>_exponential</code> transition.</li>
    <li><code>setValueAtTime</code> via the <code>_step</code> transition.</li>
  </ul>

  <p>Let's explore all of them in the example below.</p>

  <blockquote>Pro tip: When using <code>AudioNumeric</code>, consider starting with a <code>_step</code> transition. Otherwise, the transition may be abrupt and unpleasant!</blockquote>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          ( oneOf
              [ bangOn
              , delay 1000
                  $ oneOf
                      [ bang
                          $ playbackRate
                          $ AudioNumeric { n: 1.0, o: 1.0, t: _step }
                      , bang
                          $ playbackRate
                          $ AudioNumeric { n: 1.3, o: 2.0, t: _linear }
                      ]
              , delay 2500
                  $ oneOf
                      [ bang
                          $ playbackRate
                          $ AudioNumeric { n: 1.0, o: 2.5, t: _step }
                      , bang
                          $ playbackRate
                          $ AudioNumeric { n: 0.7, o: 3.5, t: _exponential }
                      ]
              ]
          )
      ]
  ]</code></pre>

  @numericEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(I()(Q(c))({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}})(o))(d.value)(wL)({numericEx:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Ot(it)(1)([le(ge)(a)(Ve(wt)(g(c))([_t(),Ou(1e3)(Ve(wt)(g(c))([Ft(c)(ia()(Lr)({n:1,o:1,t:mD})),Ft(c)(ia()(Lr)({n:1.3,o:2,t:Uo}))])),Ou(2500)(Ve(wt)(g(c))([Ft(c)(ia()(Lr)({n:1,o:2.5,t:mD})),Ft(c)(ia()(Lr)({n:.7,o:3.5,t:sC}))]))]))])])}})))})}}};var OL=function(){return d.value}(),Qh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2>AudioSudden</h2>
  <p>The simplest change you can make is scheduling a value to change <i>now</i>. This is done with <code>AudioSudden</code>, which is a wrapper around the setter for an audio parameter's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/value"><code>value</code></a> field in the Web Audio API.</p>

  <p>In the example below, we change a value after it has run for 1.5 seconds.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          ( oneOf
              [ bangOn
              , delay 1500
                  $ bang
                  $ playbackRate
                  $ AudioSudden { n: 1.4 }
              ]
          )
      ]
  ]</code></pre>

  @suddenEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(I()(Q(c))({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}})(o))(d.value)(OL)({suddenEx:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Ot(it)(1)([le(ge)(a)(Ve(wt)(g(c))([_t(),Ou(1500)(Ft(c)(ia()(_C)({n:1.4})))]))])])}})))})}}};var RL=function(){return d.value}(),Kh=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2>Audio Units</h2>
  <p>In my humble opinion, the summit of Web Audio programming is when audio units control the audio parameters of other audio units. This allows for a form of radical experimentation that is difficult in many other frameworks. <a href="https://www.w3.org/TR/webaudio/#ModularRouting">Nearly any audio parameter</a> can be automated this way.</p>

  <p>To control an audio parameter with an audio unit, use the <code>AudioUnit</code> constructor. You can also use a <code>Node D1 l p</code>. If your node is for an arbitrary number of channels, make sure to coerce it to mono using the <code>c1</code> function, as in the example below.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ loopBuf buf
      ( oneOf
          [ bangOn
          , bang
              $ playbackRate
              $ c1
                  ( gain_ 1.0
                      [ constant 1.0 bangOn
                      , gain_ 0.2 (lowpass_ 100.0 (squareOsc 50.0 bangOn))
                      ]
                  )
          ]
      )
  ]
</code></pre>

  ~unitEx~
  </section>
`}})()()(I()(Q(c))({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}})(o))(o)(RL)({unitEx:R(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([le(ge)(a)(Ve(wt)(g(c))([_t(),Ft(c)(ia()(fC(Wu)(Wu))(iC(Ot(it)(1)([bs(fs)(1)(_t()),Ot(it)(.2)([$c(ls)(100)([b_(yc)(50)(_t())])])]))))]))])}})))})}}};var LL=function(){return d.value}(),Yh=function(t){return function(e){return function(r){return function(n){var a=j(tt)(e(T_.value))(vn),u=Pa(t)(r);return $t({reflectType:function(){return`<div>
  <h1>Parameters</h1>

  <h3>Controlling our units</h3>
  <p>
    In the previous section, we saw how we can use browser events to control audio units. The Web Audio API provides a rich set of tools to control both the audio-rate and control-rate parameters of audio units. This section goes over how wags exposes those parameters.
  </p>

  ~sudden~
  ~numeric~
  ~envelope~
  ~cancel~
  ~unit~

  <h2>Next steps</h2>
  <p>In this section, we saw how to specify parameters for audio units, including using audio-rate audio units as parameters. In the next section, we'll look at how to make events <a ~next~ style="cursor:pointer;">stateful</a>.</p>
</div>`}})()()(I()(I()(_n()(I()(I()(I()(Q(c))({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}})(o))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}})(o))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}})(o))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})(o))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})(o))(o)(LL)({sudden:R(Qh(u)(e)(n)),numeric:R(Xh(u)(e)(n)),envelope:R(jh(u)(e)(n)),cancel:R(Jh(u)(e)(n)),unit:R(Kh(u)(e)(n)),next:wa(N(c))(S(o))(n)(a)})}}}};var GL=function(){return d.value}(),Zh=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<div>
  <h1>Tumult</h1>

  <h2>The unknown unknowns of web audio</h2>
  <p>
    As we saw in the last section on subgraphs, sometimes, you simply don't know how your audio will evolve. For example, if we are building a live coding environment and create a space where someone can basically do <i>anything</i>, working with anonymous audio nodes will not cut it. For example, imagine that they have two relatively unconnected parts of the audio graph and want to start creating cross-connections. With subgraphs, we can't pick apart the graph and say "take node X and connect it to node Y" as nodes don't have names.
  </p>

  <p>
    Tumult solves this problem. It is an entirely separate rendering engine from the one we've seen before that works via <i>diffing</i> two audio graphs and adjusting the current graph based on the diff. The biggest difference is that tumult <i>requires</i> you to give a name to each audio unit. Otherwise, the engine won't know what node corresponds to what name.
  </p>

  <p>
    For you veteran coders and audio-ers, you may bristle at the word <i>diff</i>. After all, diffing is expensive, and we are working in resource constrained environments where timing is of the essence. Fear not! Tumult uses PureScript <code>Set</code>-s to do blazingly fast diffs. That said, tumult is the slowest part of wags (meaning it is just blazingly fast instead of obscenely and indecently fast), so only use it where you can afford a performmance hit.
  </p>

  <h2>Hello tumult</h2>

  <h2>Stability in tumult</h2>

  <p>While tumult will run a diff every time its event is triggered, if you don't tear down a node, its nodes will merrily putter along with whatever events you send them. This means that you can trigger tumult events <i>only</i> when you need to change the shape of the graph.</p>

  <h2>Feeling lucky</h2>

  <p>So far, we have only seen the <code>DiffMe</code> instruction sent to tumult. There is another instruction you can send called <code>FeelingLucky</code>. <code>FeelingLucky</code> will attempt to perform its instruction and be a no-op if it is incoherent with respect to the graph. Careful with this, though! <code>DiffMe</code> guarantees that whatever you send to tumult will be what's rendered from your loudspeaker. Too many <code>FeelingLucky</code>-s, on the other hand, can lead to bugs where you're not quite sure anymore <i>what</i> is playing. That said, <code>FeelingLucky</code> is grotesquely fast, especially for larger graphs. So if it makes sense to use it, use it!</p>

  <h2>Next steps</h2>
  <p>In this section, we learned how to use tumult to create truly dynamic audio graphs that allow you to fine-tune the speed-versus-dynamism tradeoff in various ways.</p>
  <p>In the next section, we'll look at how to create audio graphs via an <a ~next~ style="cursor:pointer;">imperative API that more closely resembles Web Audio while providing additional type-safety benefits</a>.</p>
</div>`}})()()(_n()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))(o)(GL)({next:Y(S(o))(K(sr)(pr.value)(Qe(T(j(tt)(e(ys.value))(vn)))))})}}}};var UL=function(){return d.value}(),t0=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<div>
  <h1>Audio Worklets</h1>

  <h2>Supercharging our audio experiences</h2>

  <p>The Web Audio lets you do basically anything, but when anything isn't enough, there are custom Audio Worklets. </p>

  <p>Custom audio worklets come in a few shapes an sizes:</p>

  <ul>
    <li>You can <a href="https://developer.chrome.com/blog/audio-worklet/">write your own in JavaScript</a>. While this is ok for small PoCs, it will get really slow really quick.</li>
    <li>You can compile your favorite C/C++/Rust audio processors to <a href="https://developer.chrome.com/blog/audio-worklet-design-pattern/#using-audio-worklet-with-webassembly">web assembly</a>. The helpful folks at Google have created many examples that do exactly this, and you can pilfer them for fun and profit.</li>
    <li>My go-to solution is Faust. <a href="https://faust.grame.fr/">Faust</a> is the greatest audio project on Earth, and let's you build very performant audio processors with an expressive and concise syntax. Faust's <a href="https://webaudioconf.com/posts/2017_EA_60/">Web Audio integration</a> is a one-liner from the command line.</li>
  </ul>

  <p>In this example, we'll use Faust to create a three custom audio units and wire them together using Wags. The units are:</p>

  <ul>
    <li>A wicked sawtooth synth.</li>
    <li>A kickin' comb filter.</li>
    <li>Freeverb, the free-software-world's favorite reverb!</li>
  </ul>

  <h2>Faust</h2>

  <p>Here are our three faust examples:</p>

  <h3>Sawtooth</h3>

  <h3>Comb filter</h3>

  <h3>Freeverb</h3>

  <h3>Compilation</h3>

  We can compile each example using the following commands. Each command creates a self-contained JavaScript file, so all we need to do is link to it in wags-land.

  <pre><code>placeholder</code></pre>

  <h2>Wags</h2>

  <p>Wags provides a type-safe interface for declaring the API of Audio Worklets. While it's your job to make sure the API is in fact the actual API of the worklet, assuming this is correct, wags will enforce it for you. Let's see how in the following example. Here's the code:</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <h3>The API contract</h3>

  <p>The <code>InitializeAudioWorklet</code> API contract is defined like so:</p>

  <pre><code>InitializeAudioWorkletNode
  { name :: Proxy name
  , numberOfInputs :: numberOfInputs
  , numberOfOutputs :: numberOfOutputs
  , outputChannelCount :: outputChannelCount
  , parameterData :: { | parameterData }
  , processorOptions :: { | processorOptions }
  }</code></pre>

  <p>And here is how the APIs parameters are used:</p>

  <table>
    <tr>
      <th>Parameter</th>
      <th>Description</th>
    </tr>
    <tr>
      <td><code>name</code></td>
      <td>The name of the worklet as defined in the JS output. When in doubt, check the JS produced by faust!</td>
    </tr>
    <tr>
      <td><code>numerOfInputs</code></td>
      <td>How many inputs does the node take? Note that this is <i>not</i> the number of channels, but rather the number of distinct inputs, like in a merger node.</td>
    </tr>
    <tr>
      <td><code>numerOfOutputs</code></td>
      <td>How many outputs does the node produce? Note that this is <i>not</i> the number of channels, but rather the number of distinct outputs, like in a splitter node.</td>
    </tr>
    <tr>
      <td><code>outputChannelCount</code></td>
      <td>How many channels are in each output? This is defined using a tuple of type-level numbers.</td>
    </tr>
    <tr>
      <td><code>parameterData</code></td>
      <td>Initial parameters for the audio node. This will also define what parameters can be sent to it via events (the record gets flipped to a <a href="https://github.com/natefaubion/purescript-variant"><code>Variant</code></a> when it is an event).</td>
    </tr>
  </table>

  <h2>Next steps</h2>
  <p>In this section, we created three audio worklet nodes using Faust and used them in the Web Audio API via wags. There is active work going on to bundle all this into a single toolchain so that Faust can be written directly in PureScript and automatically read as an Audio Worklet by wags. Until that happens, though, this is a great solution: just make sure to get the parameter names right across the language barrier! No amonut of type-safety can save you there \u{1F605}</p>
  <p>In the next section, we'll look at how to create <a ~next~ style="cursor:pointer;">mutable state in a wags graph</a>.</p>
</div>`}})()()(_n()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))(o)(UL)({next:Y(S(o))(K(sr)(pr.value)(Qe(T(j(tt)(e(Lf.value))(vn)))))})}}}};var HL=function(){return d.value}(),e0=function(t){return function(e){return function(r){return function(n){return me({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out wags! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-wags">Wags GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Happy wagging!</p>
</div>`}})({reflectType:function(){return"~"}})()()(o)(Q(c))(d.value)(HL)({})}}}};var VL=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\\))
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (blank, switcher, text, text_)
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (create, fold, makeEvent, subscribe)
import FRP.Event.Class (bang)
import FRP.Event.Time (delay)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, playBuf)
import WAGS.Core (Channel(..))
import WAGS.Interpret (bracketCtx, decodeAudioDataFromUri)
import WAGS.Core (bangOn)
import WAGS.Run (run2_)
import WAGS.WebAPI (BrowserAudioBuffer)

type StartStop = V (start :: Unit, stop :: Effect Unit)
type UIEvents = V (startStop :: StartStop, slider :: Unit)

bell =
  "https://freesound.org/data/previews/339/339810_5121236-lq.mp3"
    :: String

random :: Behavior Number
random = behavior \\e ->
  makeEvent \\k -> subscribe e \\f ->
    Random.random >>= k <<< f

main :: Effect Unit
main = do
  { push, event } <- create
  runInBody (switcher scene event)
  push Nothing
  launchAff_ $ bracketCtx
    \\ctx -> decodeAudioDataFromUri ctx bell >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Element lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        startE = bang unit <|> event.startStop.start
        sl = sampleBy (/\\) random
          $ fold (\\_ b -> b + 1) event.slider 0
        music = run2_
          [ gain_ 1.0 $ map
              ( \\i ->
                  oneOf
                    [ bang $ Sound $ playBuf
                        { buffer: buffer, playbackRate: 0.7 + (fst i) * 2.0 }
                        bangOn
                    , delay 5000 $ bang $ Silence
                    ]
              )
              sl
          ]
      D.div_
        [ D.div_
            [ text_ "Slide me!"
            , D.input
                ( oneOfMap bang
                    [ D.Xtype := "range"
                    , D.Min := "0"
                    , D.Max := "100"
                    , D.Step := "1"
                    , D.Value := "50"
                    , D.OnInput := cb (const (push.slider unit))
                    ]
                )
                blank
            ]
        , D.button
            ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                [ startE $> (music >>= push.startStop.stop)
                , event.startStop.stop <#>
                    (_ *> push.startStop.start unit)
                ]
            )
            [ text $ oneOf
                [ startE $> "Turn on"
                , event.startStop.stop $> "Turn off"
                ]
            ]
        ]
`,JL=nu(function(t){return qt(function(e){return St(t)(function(r){return function(){var a=Gu();return e(r(a))()}})})}),jL=function(){return d.value}(),XL="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",r0=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels lock payload))</code>. Streaming audio is a data type with two constructors: <code>Sound (Node outputChannels lock payload)</code> to create a subgraph and <code>Silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: wags automatically frees up resources when you send the <code>Silence</code> event. Note that, once you turn a subraph off with <code>Silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(o)(I()(I()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(o))(d.value)(jL)({txt:R(or(ue)(VL)),ex1:R(rt(vu()(o)(ur({reflectSymbol:function(){return"slider"}})()()(Iu({reflectSymbol:function(){return"startStop"}})()()()(ur({reflectSymbol:function(){return"loading"}})()()(ur({reflectSymbol:function(){return"start"}})()()(ur({reflectSymbol:function(){return"stop"}})()()(Hn)()()()())()()()())()()()())(Hn)()()()())()()()())(d.value)(function(n){return function(a){var u=O(N(c))(Y(S(o))(void 0))(a.startStop.start),f=gi(S(o))(nt.create)(JL)(Pu(S(o))(function(m){return function(s){return s+1|0}})(a.slider)(0)),i=function(m){return[Ot(it)(1)([oC(p(k)(function(s){return Ve(wt)(g(c))([Y(S(o))(new ns(Qn(v_(yt(At()(z(z(bt)(kC)()()()({reflectSymbol:function(){return"playbackRate"}}))(p_)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())))({buffer:m,playbackRate:.7+Ua(s)*2})(_t()))),Ou(5e3)(Y(S(o))(as.value))])})(f))])]};return Be(o)([Be(o)([or(ue)("Slide me!"),So(o)(Nr(wt)(g(c))(Y(S(o)))([K(jo)(Eo.value)("range"),K(ef)(Ei.value)("0"),K(tf)(Ci.value)("100"),K(nf)(hi.value)("1"),K(af)(Ti.value)("50"),K(rf)(Si.value)(Qe(T(n.slider(void 0))))]))([])]),Tn(o)(Nr(wt)(g(c))(p(k)(function(){var m=K(sr)(pr.value);return function(s){return m(Qe(T(s)))}}()))([X(k)(a.startStop.loading)(h(c)(void 0)),tr(k)(a.startStop.stop)(function(m){return j(tt)(m)(j(tt)(t(h(c)(void 0)))(n.startStop.start(void 0)))}),tr(k)(On(S(o))(O(N(c))(Y(S(o))(h(c)(void 0)))(p(k)(function(m){return m.value0})(r)))(X(k)(u)(Z(et))))(function(m){return function(){m(),n.startStop.loading(void 0)();var _=go(q(Rr)(oa(Ir))(function(v){return q(Rr)(zu(Ir)(v))(function(D){return q(Rr)(Ct(v)(XL))(function(l){return mr(Ir)(function(){var ot=hs(i(l))(),vt=j(tt)(j(tt)(ot)(D))(xn(ir)(v));return n.startStop.stop(vt)(),vt})})})}))();return t(function(){return n.startStop.start(void 0)(),zo(Ai(_))()})(),void 0}})]))([ln(ue)(Ve(wt)(g(c))([p(k)(T("Turn off"))(a.startStop.stop),p(k)(T("Turn on"))(u)]))])])}})))})}}};var KL=function(){return d.value}(),n0=function(t){return function(e){return function(r){return function(n){var a=Pa(t)(r);return $t({reflectType:function(){return`<div>
  <h1>Subgraphs</h1>

  <h2>Making audio even more dynamic</h2>
  <p>
    When we're creating video games or other types of interactive work, it's rare that we'll be able to anticipate the exact web audio graph we'll need for an entire session. As an example, imagine that in a video game a certain sound effects accompany various characters, and those characters come in and out based on your progress through the game. One way to solve this would be to anticipate the maximum number of characters that are present in a game and do a round-robin assignment of nodes in the audio graph as characters enter and leave your game. But sometimes that's not ergonomic, and in certain cases its downright inefficient. Another downside is that it does not allow for specialization of the web audio graph based on new data, like for example a character to play a custom sound once you've earned a certain badge.
  </p>

  <p>
    Subgraphs fix this problem. They provide a concise mechansim to dynamically insert audio graphs based on events.
  </p>

  ~suby~

  <h2>Go forth and be brilliant!</h2>
  <p>Thus ends the first version of the wags documentation. Applause is always welcome ~appl~! Alas, some features remain undocumented, like audio worklets and an imperative API. At some point I hope to document all of these, but hopefully this should be enough to get anyone interested up and running. If you need to use any of those features before I document them, ping me on the <a href="https://purescript.org/chat">PureScript Discord</a>. Otherwise, happy music making with Wags!</p>
</div>`}})()()(I()(I()(Q(c))({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}})(o))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}})(o))(o)(KL)({appl:R(rt(Ss("\u{1F44F}")(n)(a)(function(u){return Ct(u)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(u){return function(f){return mt(u)([Ot(it)(1)([le(ge)(f)(_t())])])}}))),suby:R(r0(a)(e)(n))})}}}};var nLt=function(t){return t},aLt={Coercible0:function(){}},ZL=function(t){return function(e){var r=function(a){var u=function(f){if(f instanceof C_)return Be(o)(h(lr)(rt(Zr(o)(zh(a.setCancellation)(a.setPage)))));if(f instanceof Nf)return Be(o)(h(lr)(rt(Zr(o)(Ih(a.setCancellation)(a.setPage)))));if(f instanceof S_)return Be(o)(h(lr)(rt(Zr(o)(wh(a.setCancellation)(a.setPage)))));if(f instanceof E_)return Be(o)(h(lr)(rt(Zr(o)(PS(a.setCancellation)(a.setPage)))));if(f instanceof As)return Be(o)(h(lr)(rt(Zr(o)(t0(a.setCancellation)(a.setPage)))));if(f instanceof Lf)return Be(o)(h(lr)(rt(Zr(o)(Eh(a.setCancellation)(a.setPage)))));if(f instanceof h_)return Be(o)(h(lr)(rt(Zr(o)(Yh(a.setCancellation)(a.setPage)))));if(f instanceof T_)return Be(o)(h(lr)(rt(Zr(o)(vh(a.setCancellation)(a.setPage)))));if(f instanceof ys)return Be(o)(h(lr)(rt(Zr(o)(e0(a.setCancellation)(a.setPage)))));if(f instanceof LE)return Be(o)(h(lr)(rt(Zr(o)(Vh(a.setCancellation)(a.setPage)))));if(f instanceof x_)return Be(o)(h(lr)(rt(Zr(o)(n0(a.setCancellation)(a.setPage)))));if(f instanceof WE)return Be(o)(h(lr)(rt(Zr(o)(Zh(a.setCancellation)(a.setPage)))));throw new Error("Failed pattern match at WAGS.Example.Docs (line 144, column 5 - line 144, column 81): "+[f.constructor.name])};return u(a.page)},n=Pu(S(o))(function(a){if(a instanceof Rc)return function(u){return{prevPage:new B(u.curPage),curPage:a.value0,cancel:u.cancel,pageChange:!0}};if(a instanceof HD)return function(u){return{cancel:a.value0,pageChange:!1,curPage:u.curPage,prevPage:u.prevPage}};throw new Error("Failed pattern match at WAGS.Example.Docs (line 134, column 7 - line 136, column 75): "+[a.constructor.name])})(e)({prevPage:V.value,curPage:C_.value,cancel:h(c)(void 0),pageChange:!0});return[Be(o)(p(xe)(function(a){return vv(o)([mv(o)(O(N(c))(Nr(wt)(g(c))(Y(S(o)))([K(sr)(pr.value)(Qe(T(t(new Rc(a.value0))))),K(nk)(Jt.value)("cursor:pointer;")]))(p(k)(function(u){return K(sr)(pr.value)(Qe(T(function(){return u.cancel(),t(new Rc(a.value0))()})))})(xl(mu(c))(function(){var u=lu(Ga);return function(f){return u(function(i){return i.pageChange}(f))}}())(n))))([or(ue)(a.value1.value0)]),fc(o)(Y(S(o))(K(Ep)(Jt.value)(function(){return a.value1.value1?"":"display:none;"}())))([or(ue)(" | ")])])})([new nt(C_.value,new nt("Home",!0)),new nt(Nf.value,new nt("Hello world",!0)),new nt(S_.value,new nt("Array, fan, and fix",!0)),new nt(E_.value,new nt("Audio units",!0)),new nt(Lf.value,new nt("Events",!0)),new nt(h_.value,new nt("Parameters",!0)),new nt(T_.value,new nt("State",!0)),new nt(x_.value,new nt("Subgraphs",!1))])),Be(o)(h(lr)(Zy(o)(function(a){return r({page:a.curPage,setPage:function(u){return t(Rc.create(u))},setCancellation:function(u){return t(HD.create(u))}})})(xl(mu(c))(function(a){return a.pageChange})(n))))]}},uLt=function(t){return{page:t,setPage:Pt(Xr(Qr(un))),setCancellation:Pt(Xr(Qr(un)))}},oLt=function(){var e=q(hr)(q(hr)(ki)(ob))(ZE)();return ea(c)(Je)(p(Mr)(tS)(e))(function(r){return function(){var a=dv(),u=_r(0)(),f=Rl(o)(o)(),i=tk(o)(r)(ZL(f.push)(f.event))(Qk(u));return Kt(x)(St(i)(function(m){return m(a)}))(),f.push(new Rc(C_.value))()}})()};export{nLt as TopLevelSg,oLt as main,aLt as newtypeTopLevelSg_,uLt as p2tl,ZL as scene};
