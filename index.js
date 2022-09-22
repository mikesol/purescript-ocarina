var sy=function(t){return function(r){for(var e=r.length,n=new Array(e),a=0;a<e;a++)n[a]=t(r[a]);return n}};var ii={compose:function(t){return function(r){return function(e){return t(r(e))}}}},Ju=function(t){return t.compose},pf=function(t){return function(r){return function(e){return Ju(t)(e)(r)}}};var nt=function(t){return t.identity},at={identity:function(t){return t},Semigroupoid0:function(){return ii}};var re=!0;var yr=function(t){return function(r){return function(e){return t(e)(r)}}},E=function(t){return function(r){return t}};var Ec=function(t){return function(r){return r(t)}},sf=function(t){return function(r){return t(r)}};var d=function(){function t(){}return t.value=new t,t}();var m=function(t){return t.map},Gr=function(t){return function(r){return function(e){return m(t)(e)(r)}}},ir=function(t){return m(t)(E(void 0))},Y=function(t){return function(r){return function(e){return m(t)(E(e))(r)}}},Cp=function(t){return function(r){return m(t)(E(r))}};var Qa={map:Ju(ii)},Or={map:sy},Im=function(t){return function(r){return function(e){return m(t)(function(n){return n(e)})(r)}}};var my=function(t){return function(r){return t.length===0?r:r.length===0?t:t.concat(r)}};var ke=function(t){return t.reflectSymbol};var Sc=function(t){var r=function(e){var n;function a(u){e=u}for(;;)n=a(e);return n};return r(t)};var Ep=function(t){return function(r){return{}.hasOwnProperty.call(r,t)}},Xa=function(t){return function(r){return r[t]}},Qu=function(t){return function(r){return function(e){var n={};for(var a in e)({}).hasOwnProperty.call(e,a)&&(n[a]=e[a]);return n[t]=r,n}}};var vy={append:function(t){return function(r){return void 0}}};var Je={append:my};var Dt=function(t){return t.append},Rm=function(t){return{append:function(r){return function(e){return function(n){return Dt(t)(r(n))(e(n))}}}}};var X=function(t){return t.alt};var Dy=function(t){return function(r){for(var e=t.length,n=r.length,a=new Array(e*n),u=0,o=0;o<e;o++)for(var i=t[o],f=0;f<n;f++)a[u++]=i(r[f]);return a}};var xl={apply:Dy,Functor0:function(){return Or}},yt=function(t){return t.apply};var dt=function(t){return function(r){return function(e){return yt(t)(m(t.Functor0())(E(nt(at)))(r))(e)}}},Mn=function(t){return function(r){return function(e){return function(n){return yt(t)(m(t.Functor0())(r)(e))(n)}}}};var l=function(t){return t.pure};var Pn=function(t){return function(r){return function(e){if(r)return e;if(!r)return l(t)(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[r.constructor.name,e.constructor.name])}}},Fl=function(t){return function(r){return function(e){return yt(t.Apply0())(l(t)(r))(e)}}};var De={pure:function(t){return[t]},Apply0:function(){return xl}};var dy=function(t){return function(r){for(var e=[],n=0,a=t.length;n<a;n++)Array.prototype.push.apply(e,r(t[n]));return e}};var Eo={bind:dy,Apply0:function(){return xl}},mt=function(t){return t.bind},jn=function(t){return yr(mt(t))};var mf=function(t){return function(r){return function(e){return function(n){return mt(t)(r(n))(e)}}}};var So=function(t){return function(r){return mt(t)(r)(nt(at))}};var en=function(t){return function(r){for(var e=t>r?-1:1,n=new Array(e*(r-t)+1),a=t,u=0;a!==r;)n[u++]=a,a+=e;return n[u]=a,n}},fT=function(t){return function(r){if(t<1)return[];var e=new Array(t);return e.fill(r)}},lT=function(t){return function(r){for(var e=[],n=0,a=0;a<t;a++)e[n++]=r;return e}},Sp=typeof Array.prototype.fill=="function"?fT:lT,_T=function(){function t(a,u){this.head=a,this.tail=u}var r={};function e(a){return function(u){return new t(a,u)}}function n(a){for(var u=[],o=0,i=a;i!==r;)u[o++]=i.head,i=i.tail;return u}return function(a){return function(u){return n(a(e)(r)(u))}}}(),la=function(t){return t.length};var by=function(t){return function(r){return function(e){return function(n){return n<0||n>=e.length?r:t(e[n])}}}};var yy=function(t){return function(r){return function(e){return function(n){for(var a=0,u=n.length;a<u;a++)if(e(n[a]))return t(a);return r}}}};var Ay=function(t){return function(r){return function(e){return function(n){if(e<0||e>=n.length)return r;var a=n.slice();return a.splice(e,1),t(a)}}}};var pT=function(){function t(r,e,n,a,u,o){var i,f,p,_,s,v,c;for(i=u+(o-u>>1),i-u>1&&t(r,e,a,n,u,i),o-i>1&&t(r,e,a,n,i,o),f=u,p=i,_=u;f<i&&p<o;)s=a[f],v=a[p],c=e(r(s)(v)),c>0?(n[_++]=v,++p):(n[_++]=s,++f);for(;f<i;)n[_++]=a[f++];for(;p<o;)n[_++]=a[p++]}return function(r){return function(e){return function(n){var a;return n.length<2?n:(a=n.slice(0),t(r,e,a,n.slice(0),0,n.length),a)}}}}();var Tc=function(t){return function(r){return function(e){for(var n=r.length<e.length?r.length:e.length,a=new Array(n),u=0;u<n;u++)a[u]=t(r[u])(e[u]);return a}}};var ky=function(t){return function(r){return t[r]}};var mT=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var gy={defer:function(t){return function(r){return t(void 0)(r)}}},vf=function(t){return t.defer},Bm=function(t){return function(r){var e=mT("go","Control.Lazy",function(){return vf(t)(function(a){return r(e(25))})}),n=e(25);return n}};var su=function(t){return function(r){return function(e){return mt(t.Bind1())(r)(function(n){return mt(t.Bind1())(e)(function(a){return l(t.Applicative0())(n(a))})})}}};var vT=String.fromCharCode(65535),DT=String.fromCharCode(0),dT=Number.POSITIVE_INFINITY,bT=Number.NEGATIVE_INFINITY;var Cy=function(t){return function(r){return function(e){return function(n){return function(a){return n<a?t:n===a?r:e}}}}};var hy=Cy,Ey=Cy;var Sy=function(t){return function(r){return t===r}};var Ty=Sy,xy=Sy;var Ol={eq:xy},ci={eq:Ty};var Vt=function(t){return t.eq};var Zt=function(){function t(){}return t.value=new t,t}(),sr=function(){function t(){}return t.value=new t,t}(),mr=function(){function t(){}return t.value=new t,t}();var Fy=function(t){return function(r){return t-r|0}},Oy=function(t){return function(r){return t-r}};var $y=function(t){return function(r){return t+r|0}},wy=function(t){return function(r){return t*r|0}},My=function(t){return function(r){return t+r}},Py=function(t){return function(r){return t*r}};var _a=function(t){return t.zero};var Sa={add:My,zero:0,mul:Py,one:1},Ka={add:$y,zero:0,mul:wy,one:1};var pa=function(t){return t.one};var Sn=function(t){return t.mul};var Mr=function(t){return t.add};var mu=function(t){return t.sub};var xc={sub:Oy,Semiring0:function(){return Sa}},Wm={sub:Fy,Semiring0:function(){return Ka}};var $l=function(t){return function(r){return mu(t)(_a(t.Semiring0()))(r)}};var La=function(){return{compare:Ey(Zt.value)(mr.value)(sr.value),Eq0:function(){return Ol}}}(),Le=function(){return{compare:hy(Zt.value)(mr.value)(sr.value),Eq0:function(){return ci}}}();var Kt=function(t){return t.compare};var Ry=function(t){return function(r){return function(e){var n=Kt(t)(r)(e);return!(n instanceof Zt)}}};var Mu=function(t){return function(r){return function(e){var n=Kt(t)(r)(e);if(n instanceof Zt)return e;if(n instanceof mr||n instanceof sr)return r;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[n.constructor.name])}}};var qm=function(t){return function(r){return function(e){var n=Ry(t)(e)(_a(r.Semiring0()));return n?e:$l(r)(e)}}};var Qn=function(t){return t.top};var Oc={top:2147483647,bottom:-2147483648,Ord0:function(){return Le}};var Xn=function(t){return t.bottom};var Ly=function(t){return t.toString()};var Ba={show:Ly};var Gt=function(t){return t.show};var w=function(){function t(){}return t.value=new t,t}(),S=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Ut=function(t){return function(r){return function(e){if(e instanceof w)return t;if(e instanceof S)return r(e.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}};var $e={map:function(t){return function(r){return r instanceof S?new S(t(r.value0)):w.value}}};var sa=function(t){return Ut(t)(nt(at))},Yn=function(){return function(t){if(t instanceof S)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var Hi={apply:function(t){return function(r){if(t instanceof S)return m($e)(t.value0)(r);if(t instanceof w)return w.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,r.constructor.name])}},Functor0:function(){return $e}},ma={bind:function(t){return function(r){if(t instanceof S)return r(t.value0);if(t instanceof w)return w.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,r.constructor.name])}},Apply0:function(){return Hi}};var To=function(){return{pure:S.create,Apply0:function(){return Hi}}}();var tr=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),rr=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var wc={map:function(t){return function(r){if(r instanceof tr)return new tr(r.value0);if(r instanceof rr)return new rr(t(r.value0));throw new Error("Failed pattern match at Data.Either (line 31, column 1 - line 31, column 52): "+[r.constructor.name])}}};var Wa=function(t){return function(r){return function(e){if(e instanceof tr)return t(e.value0);if(e instanceof rr)return r(e.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},Tp=function(){return Wa(E(w.value))(S.create)}();var vu=function(t){return t};var Xu={map:function(t){return function(r){return t(r)}}};var By={apply:function(t){return function(r){return t(r)}},Functor0:function(){return Xu}},NT={bind:function(t){return function(r){return r(t)}},Apply0:function(){return By}},zm={pure:vu,Apply0:function(){return By}},Pu={Applicative0:function(){return zm},Bind1:function(){return NT}};var Wy=function(t){return Math.min(Math.abs(t),2147483647)},Uy=function(t){return function(r){return r===0?0:r>0?Math.floor(t/r):-Math.floor(t/-r)}},qy=function(t){return function(r){if(r===0)return 0;var e=Math.abs(r);return(t%e+e)%e}},Hy=function(t){return function(r){return t/r}};var zy={Ring0:function(){return xc}},Vy={Ring0:function(){return Wm}};var Za=function(t){return t.mod};var Nl={degree:function(t){return 1},div:Hy,mod:function(t){return function(r){return 0}},CommutativeRing0:function(){return zy}},Ku={degree:Wy,div:Uy,mod:qy,CommutativeRing0:function(){return Vy}},Yu=function(t){return t.div};var zi={mempty:void 0,Semigroup0:function(){return vy}};var vr=function(t){return t.mempty},xp=function(t){return{mempty:function(r){return vr(t)},Semigroup0:function(){return Rm(t.Semigroup0())}}};var Vm=function(t){return function(){return t}},Gy=function(t){return function(r){return function(){return r(t())()}}};var Zu=function(t){return function(r){return function(){for(var e=0,n=t.length;e<n;e++)r(t[e])()}}};var Jy=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},fi={Applicative0:function(){return vt},Bind1:function(){return Zn}},Zn={bind:Gy,Apply0:function(){return Gm(0)}},vt={pure:Vm,Apply0:function(){return Gm(0)}},jy=Jy("functorEffect","Effect",function(){return{map:Fl(vt)}}),Gm=Jy("applyEffect","Effect",function(){return{apply:su(fi),Functor0:function(){return jy(0)}}}),N=jy(20),kt=Gm(23),Jm=function(t){return{append:Mn(kt)(Dt(t))}},Vi=function(t){return{mempty:Vm(vr(t)),Semigroup0:function(){return Jm(t.Semigroup0())}}};var Qy=function(t){return function(){return{value:t}}};var Jr=function(t){return function(){return t.value}},Xy=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},ne=function(t){return function(r){return function(){r.value=t}}};var Br=Qy,HT=Xy,li=function(t){return HT(function(r){var e=t(r);return{state:e,value:e}})},_i=function(t){return function(r){return ir(N)(li(t)(r))}};var Ky=function(t){return function(r){return function(){return t(r())}}},Yy=function(t){return function(){return t}},Zy=function(t){return function(r){return function(){return r(t())()}}};function je(t){return function(){return{value:t}}}var Tn=function(t){return function(){return t.value}},tA=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},du=function(t){return function(r){return function(){return r.value=t}}};var QT=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},XT=tA,eo=function(t){return XT(function(r){var e=t(r);return{state:e,value:e}})},xe={map:Ky},Qm={Applicative0:function(){return _r},Bind1:function(){return Mc}},Mc={bind:Zy,Apply0:function(){return Xm(0)}},_r={pure:Yy,Apply0:function(){return Xm(0)}},Xm=QT("applyST","Control.Monad.ST.Internal",function(){return{apply:su(Qm),Functor0:function(){return xe}}}),$p=Xm(46);function Pc(){return[]}var Km=function(t){return function(r){return function(){return r.push.apply(r,t)}}};var wp=function(t){return function(r){return function(e){return function(n){return function(){return n.splice.apply(n,[t,r].concat(e))}}}}};function KT(t){return function(){return t.slice()}}var Mp=KT;var YT=function(){function t(r,e,n,a,u,o){var i,f,p,_,s,v,c;for(i=u+(o-u>>1),i-u>1&&t(r,e,a,n,u,i),o-i>1&&t(r,e,a,n,i,o),f=u,p=i,_=u;f<i&&p<o;)s=a[f],v=a[p],c=e(r(s)(v)),c>0?(n[_++]=v,++p):(n[_++]=s,++f);for(;f<i;)n[_++]=a[f++];for(;p<o;)n[_++]=a[p++]}return function(r){return function(e){return function(n){return function(){return n.length<2||t(r,e,n,n.slice(0),0,n.length),n}}}}}();var Gi=function(t){return Km([t])};var uA=function(t){return function(r){return t&&r}},oA=function(t){return function(r){return t||r}},iA=function(t){return!t};var Iu=function(t){return t.not};var Ji=function(t){return t.disj},bu={ff:!1,tt:!0,implies:function(t){return function(r){return Ji(bu)(Iu(bu)(t))(r)}},conj:uA,disj:oA,not:iA};var fA=function(t){return function(r){return function(e){for(var n=r,a=e.length,u=a-1;u>=0;u--)n=t(e[u])(n);return n}}},lA=function(t){return function(r){return function(e){for(var n=r,a=e.length,u=0;u<a;u++)n=t(n)(e[u]);return n}}};var T=function(t){return t.empty};var rt=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),Af=function(t){return function(r){return t(r.value0)(r.value1)}};var Qe=function(t){return t.value1};var xo={map:function(t){return function(r){return new rt(r.value0,t(r.value1))}}};var qa=function(t){return t.value0};var Ic=function(t){return function(r){return function(e){return t(new rt(r,e))}}};var pt=function(t){return t};var O=function(){return pt};var Xe=O,ae=O;var ev=function(){return function(){return function(t){return O()}}};var ue=function(t){return t.foldr};var Yt=function(t){return function(r){return ue(t)(X(r.Alt0()))(T(r))}},_n=function(t){return function(r){return function(e){return ue(t)(function(){var n=X(r.Alt0());return function(a){return n(e(a))}}())(T(r))}}},Ie=function(t){return function(r){return function(e){return ue(r)(function(){var n=dt(t.Apply0());return function(a){return n(e(a))}}())(l(t)(void 0))}}},pn=function(t){return function(r){return yr(Ie(t)(r))}};var pe=function(t){return t.foldl};var Zr={foldr:function(t){return function(r){return function(e){if(e instanceof w)return r;if(e instanceof S)return t(e.value0)(r);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof w)return r;if(e instanceof S)return t(r)(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldMap:function(t){return function(r){return function(e){if(e instanceof w)return vr(t);if(e instanceof S)return r(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[r.constructor.name,e.constructor.name])}}}};var vA=function(t){return function(r){return function(e){return ue(t)(function(n){return function(a){return Dt(r.Semigroup0())(e(n))(a)}})(vr(r))}}},_t={foldr:fA,foldl:lA,foldMap:function(t){return vA(_t)(t)}};var sn=function(t){return t.foldMap};var DA=function(){function t(a){return[a]}function r(a){return function(u){return[a,u]}}function e(a){return function(u){return function(o){return[a,u,o]}}}function n(a){return function(u){return a.concat(u)}}return function(a){return function(u){return function(o){return function(i){return function(f){function p(_,s){switch(s-_){case 0:return o([]);case 1:return u(t)(i(f[_]));case 2:return a(u(r)(i(f[_])))(i(f[_+1]));case 3:return a(a(u(e)(i(f[_])))(i(f[_+1])))(i(f[_+2]));default:var v=_+Math.floor((s-_)/4)*2;return a(u(n)(p(_,v)))(p(v,s))}}return p(0,f.length)}}}}}}();var Rn=function(t){return t.traverse};var TA=function(t){return function(r){return Rn(t)(r)(nt(at))}},Oo={traverse:function(t){return DA(yt(t.Apply0()))(m(t.Apply0().Functor0()))(l(t))},sequence:function(t){return TA(Oo)(t)},Functor0:function(){return Or},Foldable1:function(){return _t}};var Ql=function(){return Tc(rt.create)}();var Dv=function(){return ky};var MA=function(t){return[t]};var G=function(t){return function(r){return Tc(t)(en(0)(la(r)-1|0))(r)}};var Xl=function(){return by(S.create)(w.value)}(),dv=function(t){return Xl(t)(la(t)-1|0)};var PA=function(){return yy(S.create)(w.value)}();var bv=function(){return Ay(S.create)(w.value)}(),Kl=function(t){return function(r){return function(e){return e.length===0?[]:Ut(e)(function(n){return Yn()(bv(n)(e))})(PA(t(r))(e))}}};var Xi=function(t){return function(r){return Dt(Je)([t])(r)}};var IA=function(t){return function(r){for(var e=r.length,n=Array(e),a=0;a<e;a++)n[a]=t(a)(r[a]);return n}};var $o=function(t){return t.mapWithIndex};var mi={mapWithIndex:IA,Functor0:function(){return Or}};var wo=function(t){return t.foldrWithIndex};var ao=function(t){return t.foldlWithIndex};var vi=function(t){return t.foldMapWithIndex};var Ki=function(t){return t.traverseWithIndex};var uo=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var zp=function(t){return function(r){return new uo(r,T(t))}};var Fe=function(){function t(){}return t.value=new t,t}(),cr=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),Vp=function(t){return t},EF=function(t){return new cr(t.value0,t.value1)};var SF=function(t){var r=function(e){return function(n){var a=e,u=!1,o;function i(f,p){if(p instanceof cr&&p.value1 instanceof cr&&p.value1.value1 instanceof cr){a=new cr(p,f),n=p.value1.value1.value1;return}var _=function(v){return v instanceof cr&&v.value1 instanceof cr&&v.value1.value1 instanceof Fe?new cr(t(v.value0),new cr(t(v.value1.value0),Fe.value)):v instanceof cr&&v.value1 instanceof Fe?new cr(t(v.value0),Fe.value):Fe.value},s=function(v){return function(c){var D=v,M=!1,et;function Ft(Wt,Xr){if(Wt instanceof cr&&Wt.value0 instanceof cr&&Wt.value0.value1 instanceof cr&&Wt.value0.value1.value1 instanceof cr){D=Wt.value1,c=new cr(t(Wt.value0.value0),new cr(t(Wt.value0.value1.value0),new cr(t(Wt.value0.value1.value1.value0),Xr)));return}return M=!0,Xr}for(;!M;)et=Ft(D,c);return et}};return u=!0,s(f)(_(p))}for(;!u;)o=i(a,n);return o}};return r(Fe.value)},Yl={map:SF};var Ha={foldr:function(t){return function(r){var e=function(){var a=function(u){return function(o){var i=u,f=!1,p;function _(s,v){if(v instanceof Fe)return f=!0,s;if(v instanceof cr){i=new cr(v.value0,s),o=v.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[s.constructor.name,v.constructor.name])}for(;!f;)p=_(i,o);return p}};return a(Fe.value)}(),n=pe(Ha)(yr(t))(r);return function(a){return n(e(a))}}},foldl:function(t){var r=function(e){return function(n){var a=e,u=!1,o;function i(f,p){if(p instanceof Fe)return u=!0,f;if(p instanceof cr){a=t(f)(p.value0),n=p.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[p.constructor.name])}for(;!u;)o=i(a,n);return o}};return r},foldMap:function(t){return function(r){return pe(Ha)(function(e){var n=Dt(t.Semigroup0())(e);return function(a){return n(r(a))}})(vr(t))}}};var Zl={append:function(t){return function(r){return ue(Ha)(cr.create)(r)(t)}}};var kv={append:function(t){return function(r){return new uo(t.value0,Dt(Zl)(t.value1)(EF(r)))}}};var NA={alt:Dt(Zl),Functor0:function(){return Yl}},gv=function(){return{empty:Fe.value,Alt0:function(){return NA}}}();var HA=function(t){return t()};var zA=function(t){throw new Error(t)};var VA=function(){return zA};var jF=HA,Au=function(t){return jF(function(){return VA()(t)})};var Qt=function(){function t(){}return t.value=new t,t}(),Dr=function(){function t(r,e,n,a){this.value0=r,this.value1=e,this.value2=n,this.value3=a}return t.create=function(r){return function(e){return function(n){return function(a){return new t(r,e,n,a)}}}},t}(),Ur=function(){function t(r,e,n,a,u,o,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=o,this.value6=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return function(i){return new t(r,e,n,a,u,o,i)}}}}}}},t}(),Yi=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),di=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),Zi=function(){function t(r,e,n,a,u,o){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return new t(r,e,n,a,u,o)}}}}}},t}(),Ro=function(){function t(r,e,n,a,u,o){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return new t(r,e,n,a,u,o)}}}}}},t}(),tc=function(){function t(r,e,n,a,u,o){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return new t(r,e,n,a,u,o)}}}}}},t}(),Jp=function(){function t(r,e,n,a){this.value0=r,this.value1=e,this.value2=n,this.value3=a}return t.create=function(r){return function(e){return function(n){return function(a){return new t(r,e,n,a)}}}},t}();var jA=function(t){return function(r){return new Dr(Qt.value,t,r,Qt.value)}};var Qp=function(t){return function(r){var e=Kt(t),n=function(a){var u=!1,o;function i(f){if(f instanceof Qt)return u=!0,w.value;if(f instanceof Dr){var p=e(r)(f.value1);if(p instanceof mr)return u=!0,new S(f.value2);if(p instanceof Zt){a=f.value0;return}a=f.value3;return}if(f instanceof Ur){var _=e(r)(f.value1);if(_ instanceof mr)return u=!0,new S(f.value2);var s=e(r)(f.value4);if(s instanceof mr)return u=!0,new S(f.value5);if(_ instanceof Zt){a=f.value0;return}if(s instanceof sr){a=f.value6;return}a=f.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[f.constructor.name])}for(;!u;)o=i(a);return o};return n}};var QA=function(t){return t instanceof Qt};var an=function(t){return function(r){return function(e){var n=t,a=r,u=!1,o;function i(f,p,_){if(p instanceof Fe)return u=!0,_;if(p instanceof cr){if(p.value0 instanceof Yi){n=f,a=p.value1,e=new Dr(_,p.value0.value0,p.value0.value1,p.value0.value2);return}if(p.value0 instanceof di){n=f,a=p.value1,e=new Dr(p.value0.value0,p.value0.value1,p.value0.value2,_);return}if(p.value0 instanceof Zi){n=f,a=p.value1,e=new Ur(_,p.value0.value0,p.value0.value1,p.value0.value2,p.value0.value3,p.value0.value4,p.value0.value5);return}if(p.value0 instanceof Ro){n=f,a=p.value1,e=new Ur(p.value0.value0,p.value0.value1,p.value0.value2,_,p.value0.value3,p.value0.value4,p.value0.value5);return}if(p.value0 instanceof tc){n=f,a=p.value1,e=new Ur(p.value0.value0,p.value0.value1,p.value0.value2,p.value0.value3,p.value0.value4,p.value0.value5,_);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[p.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[p.constructor.name,_.constructor.name])}for(;!u;)o=i(n,a,e);return o}}},r_=function(t){return function(r){return function(e){var n=function(o){return function(i){var f=o,p=!1,_;function s(v,c){if(v instanceof Fe)return p=!0,new Dr(c.value0,c.value1,c.value2,c.value3);if(v instanceof cr){if(v.value0 instanceof Yi)return p=!0,an(t)(v.value1)(new Ur(c.value0,c.value1,c.value2,c.value3,v.value0.value0,v.value0.value1,v.value0.value2));if(v.value0 instanceof di)return p=!0,an(t)(v.value1)(new Ur(v.value0.value0,v.value0.value1,v.value0.value2,c.value0,c.value1,c.value2,c.value3));if(v.value0 instanceof Zi){f=v.value1,i=new Jp(new Dr(c.value0,c.value1,c.value2,c.value3),v.value0.value0,v.value0.value1,new Dr(v.value0.value2,v.value0.value3,v.value0.value4,v.value0.value5));return}if(v.value0 instanceof Ro){f=v.value1,i=new Jp(new Dr(v.value0.value0,v.value0.value1,v.value0.value2,c.value0),c.value1,c.value2,new Dr(c.value3,v.value0.value3,v.value0.value4,v.value0.value5));return}if(v.value0 instanceof tc){f=v.value1,i=new Jp(new Dr(v.value0.value0,v.value0.value1,v.value0.value2,v.value0.value3),v.value0.value4,v.value0.value5,new Dr(c.value0,c.value1,c.value2,c.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[v.value0.constructor.name,c.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[v.constructor.name,c.constructor.name])}for(;!p;)_=s(f,i);return _}},a=Kt(t),u=function(o){return function(i){var f=o,p=!1,_;function s(v,c){if(c instanceof Qt)return p=!0,n(v)(new Jp(Qt.value,r,e,Qt.value));if(c instanceof Dr){var D=a(r)(c.value1);if(D instanceof mr)return p=!0,an(t)(v)(new Dr(c.value0,r,e,c.value3));if(D instanceof Zt){f=new cr(new Yi(c.value1,c.value2,c.value3),v),i=c.value0;return}f=new cr(new di(c.value0,c.value1,c.value2),v),i=c.value3;return}if(c instanceof Ur){var M=a(r)(c.value1);if(M instanceof mr)return p=!0,an(t)(v)(new Ur(c.value0,r,e,c.value3,c.value4,c.value5,c.value6));var et=a(r)(c.value4);if(et instanceof mr)return p=!0,an(t)(v)(new Ur(c.value0,c.value1,c.value2,c.value3,r,e,c.value6));if(M instanceof Zt){f=new cr(new Zi(c.value1,c.value2,c.value3,c.value4,c.value5,c.value6),v),i=c.value0;return}if(M instanceof sr&&et instanceof Zt){f=new cr(new Ro(c.value0,c.value1,c.value2,c.value4,c.value5,c.value6),v),i=c.value3;return}f=new cr(new tc(c.value0,c.value1,c.value2,c.value3,c.value4,c.value5),v),i=c.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[v.constructor.name,c.constructor.name])}for(;!p;)_=s(f,i);return _}};return u(Fe.value)}}},ZF=function(t){return function(r){var e=function(i){return function(f){var p=i,_=!1,s;function v(c,D){if(c instanceof Fe)return _=!0,D;if(c instanceof cr){if(c.value0 instanceof Yi&&c.value0.value2 instanceof Qt&&D instanceof Qt)return _=!0,an(t)(c.value1)(new Dr(Qt.value,c.value0.value0,c.value0.value1,Qt.value));if(c.value0 instanceof di&&c.value0.value0 instanceof Qt&&D instanceof Qt)return _=!0,an(t)(c.value1)(new Dr(Qt.value,c.value0.value1,c.value0.value2,Qt.value));if(c.value0 instanceof Yi&&c.value0.value2 instanceof Dr){p=c.value1,f=new Ur(D,c.value0.value0,c.value0.value1,c.value0.value2.value0,c.value0.value2.value1,c.value0.value2.value2,c.value0.value2.value3);return}if(c.value0 instanceof di&&c.value0.value0 instanceof Dr){p=c.value1,f=new Ur(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3,c.value0.value1,c.value0.value2,D);return}return c.value0 instanceof Yi&&c.value0.value2 instanceof Ur?(_=!0,an(t)(c.value1)(new Dr(new Dr(D,c.value0.value0,c.value0.value1,c.value0.value2.value0),c.value0.value2.value1,c.value0.value2.value2,new Dr(c.value0.value2.value3,c.value0.value2.value4,c.value0.value2.value5,c.value0.value2.value6)))):c.value0 instanceof di&&c.value0.value0 instanceof Ur?(_=!0,an(t)(c.value1)(new Dr(new Dr(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3),c.value0.value0.value4,c.value0.value0.value5,new Dr(c.value0.value0.value6,c.value0.value1,c.value0.value2,D)))):c.value0 instanceof Zi&&c.value0.value2 instanceof Qt&&c.value0.value5 instanceof Qt&&D instanceof Qt?(_=!0,an(t)(c.value1)(new Ur(Qt.value,c.value0.value0,c.value0.value1,Qt.value,c.value0.value3,c.value0.value4,Qt.value))):c.value0 instanceof Ro&&c.value0.value0 instanceof Qt&&c.value0.value5 instanceof Qt&&D instanceof Qt?(_=!0,an(t)(c.value1)(new Ur(Qt.value,c.value0.value1,c.value0.value2,Qt.value,c.value0.value3,c.value0.value4,Qt.value))):c.value0 instanceof tc&&c.value0.value0 instanceof Qt&&c.value0.value3 instanceof Qt&&D instanceof Qt?(_=!0,an(t)(c.value1)(new Ur(Qt.value,c.value0.value1,c.value0.value2,Qt.value,c.value0.value4,c.value0.value5,Qt.value))):c.value0 instanceof Zi&&c.value0.value2 instanceof Dr?(_=!0,an(t)(c.value1)(new Dr(new Ur(D,c.value0.value0,c.value0.value1,c.value0.value2.value0,c.value0.value2.value1,c.value0.value2.value2,c.value0.value2.value3),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof Ro&&c.value0.value0 instanceof Dr?(_=!0,an(t)(c.value1)(new Dr(new Ur(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3,c.value0.value1,c.value0.value2,D),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof Ro&&c.value0.value5 instanceof Dr?(_=!0,an(t)(c.value1)(new Dr(c.value0.value0,c.value0.value1,c.value0.value2,new Ur(D,c.value0.value3,c.value0.value4,c.value0.value5.value0,c.value0.value5.value1,c.value0.value5.value2,c.value0.value5.value3)))):c.value0 instanceof tc&&c.value0.value3 instanceof Dr?(_=!0,an(t)(c.value1)(new Dr(c.value0.value0,c.value0.value1,c.value0.value2,new Ur(c.value0.value3.value0,c.value0.value3.value1,c.value0.value3.value2,c.value0.value3.value3,c.value0.value4,c.value0.value5,D)))):c.value0 instanceof Zi&&c.value0.value2 instanceof Ur?(_=!0,an(t)(c.value1)(new Ur(new Dr(D,c.value0.value0,c.value0.value1,c.value0.value2.value0),c.value0.value2.value1,c.value0.value2.value2,new Dr(c.value0.value2.value3,c.value0.value2.value4,c.value0.value2.value5,c.value0.value2.value6),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof Ro&&c.value0.value0 instanceof Ur?(_=!0,an(t)(c.value1)(new Ur(new Dr(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3),c.value0.value0.value4,c.value0.value0.value5,new Dr(c.value0.value0.value6,c.value0.value1,c.value0.value2,D),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof Ro&&c.value0.value5 instanceof Ur?(_=!0,an(t)(c.value1)(new Ur(c.value0.value0,c.value0.value1,c.value0.value2,new Dr(D,c.value0.value3,c.value0.value4,c.value0.value5.value0),c.value0.value5.value1,c.value0.value5.value2,new Dr(c.value0.value5.value3,c.value0.value5.value4,c.value0.value5.value5,c.value0.value5.value6)))):c.value0 instanceof tc&&c.value0.value3 instanceof Ur?(_=!0,an(t)(c.value1)(new Ur(c.value0.value0,c.value0.value1,c.value0.value2,new Dr(c.value0.value3.value0,c.value0.value3.value1,c.value0.value3.value2,c.value0.value3.value3),c.value0.value3.value4,c.value0.value3.value5,new Dr(c.value0.value3.value6,c.value0.value4,c.value0.value5,D)))):(_=!0,Au("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[c.constructor.name])}for(;!_;)s=v(p,f);return s}},n=function(i){return function(f){var p=i,_=!1,s;function v(c,D){if(D instanceof Dr&&D.value0 instanceof Qt&&D.value3 instanceof Qt)return _=!0,e(c)(Qt.value);if(D instanceof Dr){p=new cr(new di(D.value0,D.value1,D.value2),c),f=D.value3;return}if(D instanceof Ur&&D.value0 instanceof Qt&&D.value3 instanceof Qt&&D.value6 instanceof Qt)return _=!0,e(new cr(new di(Qt.value,D.value1,D.value2),c))(Qt.value);if(D instanceof Ur){p=new cr(new tc(D.value0,D.value1,D.value2,D.value3,D.value4,D.value5),c),f=D.value6;return}return _=!0,Au("The impossible happened in partial function `removeMaxNode`.")}for(;!_;)s=v(p,f);return s}},a=function(i){var f=!1,p;function _(s){if(s instanceof Dr&&s.value3 instanceof Qt)return f=!0,{key:s.value1,value:s.value2};if(s instanceof Dr){i=s.value3;return}if(s instanceof Ur&&s.value6 instanceof Qt)return f=!0,{key:s.value4,value:s.value5};if(s instanceof Ur){i=s.value6;return}return f=!0,Au("The impossible happened in partial function `maxNode`.")}for(;!f;)p=_(i);return p},u=Kt(t),o=function(i){return function(f){var p=i,_=!1,s;function v(c,D){if(D instanceof Qt)return _=!0,w.value;if(D instanceof Dr){var M=u(r)(D.value1);if(D.value3 instanceof Qt&&M instanceof mr)return _=!0,new S(new rt(D.value2,e(c)(Qt.value)));if(M instanceof mr){var et=a(D.value0);return _=!0,new S(new rt(D.value2,n(new cr(new Yi(et.key,et.value,D.value3),c))(D.value0)))}if(M instanceof Zt){p=new cr(new Yi(D.value1,D.value2,D.value3),c),f=D.value0;return}p=new cr(new di(D.value0,D.value1,D.value2),c),f=D.value3;return}if(D instanceof Ur){var Ft=function(){return D.value0 instanceof Qt&&D.value3 instanceof Qt&&D.value6 instanceof Qt}(),M=u(r)(D.value4),Wt=u(r)(D.value1);if(Ft&&Wt instanceof mr)return _=!0,new S(new rt(D.value2,an(t)(c)(new Dr(Qt.value,D.value4,D.value5,Qt.value))));if(Ft&&M instanceof mr)return _=!0,new S(new rt(D.value5,an(t)(c)(new Dr(Qt.value,D.value1,D.value2,Qt.value))));if(Wt instanceof mr){var et=a(D.value0);return _=!0,new S(new rt(D.value2,n(new cr(new Zi(et.key,et.value,D.value3,D.value4,D.value5,D.value6),c))(D.value0)))}if(M instanceof mr){var et=a(D.value3);return _=!0,new S(new rt(D.value5,n(new cr(new Ro(D.value0,D.value1,D.value2,et.key,et.value,D.value6),c))(D.value3)))}if(Wt instanceof Zt){p=new cr(new Zi(D.value1,D.value2,D.value3,D.value4,D.value5,D.value6),c),f=D.value0;return}if(Wt instanceof sr&&M instanceof Zt){p=new cr(new Ro(D.value0,D.value1,D.value2,D.value4,D.value5,D.value6),c),f=D.value3;return}p=new cr(new tc(D.value0,D.value1,D.value2,D.value3,D.value4,D.value5),c),f=D.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[D.constructor.name])}for(;!_;)s=v(p,f);return s}};return o(Fe.value)}},va={foldr:function(t){return function(r){return function(e){if(e instanceof Qt)return r;if(e instanceof Dr)return ue(va)(t)(t(e.value2)(ue(va)(t)(r)(e.value3)))(e.value0);if(e instanceof Ur)return ue(va)(t)(t(e.value2)(ue(va)(t)(t(e.value5)(ue(va)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof Qt)return r;if(e instanceof Dr)return pe(va)(t)(t(pe(va)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Ur)return pe(va)(t)(t(pe(va)(t)(t(pe(va)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[e.constructor.name])}}},foldMap:function(t){return function(r){return function(e){if(e instanceof Qt)return vr(t);if(e instanceof Dr)return Dt(t.Semigroup0())(sn(va)(t)(r)(e.value0))(Dt(t.Semigroup0())(r(e.value2))(sn(va)(t)(r)(e.value3)));if(e instanceof Ur)return Dt(t.Semigroup0())(sn(va)(t)(r)(e.value0))(Dt(t.Semigroup0())(r(e.value2))(Dt(t.Semigroup0())(sn(va)(t)(r)(e.value3))(Dt(t.Semigroup0())(r(e.value5))(sn(va)(t)(r)(e.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[e.constructor.name])}}}},Da={foldrWithIndex:function(t){return function(r){return function(e){if(e instanceof Qt)return r;if(e instanceof Dr)return wo(Da)(t)(t(e.value1)(e.value2)(wo(Da)(t)(r)(e.value3)))(e.value0);if(e instanceof Ur)return wo(Da)(t)(t(e.value1)(e.value2)(wo(Da)(t)(t(e.value4)(e.value5)(wo(Da)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[e.constructor.name])}}},foldlWithIndex:function(t){return function(r){return function(e){if(e instanceof Qt)return r;if(e instanceof Dr)return ao(Da)(t)(t(e.value1)(ao(Da)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Ur)return ao(Da)(t)(t(e.value4)(ao(Da)(t)(t(e.value1)(ao(Da)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[e.constructor.name])}}},foldMapWithIndex:function(t){return function(r){return function(e){if(e instanceof Qt)return vr(t);if(e instanceof Dr)return Dt(t.Semigroup0())(vi(Da)(t)(r)(e.value0))(Dt(t.Semigroup0())(r(e.value1)(e.value2))(vi(Da)(t)(r)(e.value3)));if(e instanceof Ur)return Dt(t.Semigroup0())(vi(Da)(t)(r)(e.value0))(Dt(t.Semigroup0())(r(e.value1)(e.value2))(Dt(t.Semigroup0())(vi(Da)(t)(r)(e.value3))(Dt(t.Semigroup0())(r(e.value4)(e.value5))(vi(Da)(t)(r)(e.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[e.constructor.name])}}},Foldable0:function(){return va}},XA=function(){return wo(Da)(function(t){return function(r){return function(e){return new cr(t,e)}}})(Fe.value)}();var rc=function(){return Qt.value}();var Tv=function(t){return function(r){return function(e){return Ut(e)(Qe)(ZF(t)(r)(e))}}};var ec=function(t){return function(r){return function(e){return function(n){var a=r(Qp(t)(e)(n));if(a instanceof w)return Tv(t)(e)(n);if(a instanceof S)return r_(t)(e)(a.value0)(n);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[a.constructor.name])}}}};var tO=function(t){return function(r){return function(e){return function(n){var a=function(u){return function(o){return function(i){return ec(t)(function(){var f=Ut(i)(r(i));return function(p){return S.create(f(p))}}())(u)(o)}}};return ao(Da)(a)(n)(e)}}}};var KA=function(t){return tO(t)(E)};var a_=function(t){return t.partitionMap};var bi=function(t){return t.filterMap};var Tf=function(t){return t.filter};var rk=pt;var ra={liftST:rk,Monad0:function(){return fi}},ea=function(t){return t.liftST};var cO=function(t){return function(r){return function(e){return KA(t)(r)(e)}}};var xv=function(t){return XA(t)};var ak=function(t){return jA(t)(void 0)};var Fv=function(t){return{append:cO(t)}};var uk=function(t){return QA(t)},ok=function(t){return function(r){return function(e){return r_(t)(r)(void 0)(e)}}};var ik={foldMap:function(t){return function(r){var e=sn(Ha)(t)(r);return function(n){return e(xv(n))}}},foldl:function(t){return function(r){var e=pe(Ha)(t)(r);return function(n){return e(xv(n))}}},foldr:function(t){return function(r){var e=ue(Ha)(t)(r);return function(n){return e(xv(n))}}}};var Ov=rc;var ck=function(t){return{mempty:Ov,Semigroup0:function(){return Fv(t)}}};var Xp=function(t){return function(r){return function(e){return Tv(t)(r)(e)}}};function fk(t){return function(r){return function(){return setTimeout(r,t)}}}function lk(t){return function(){clearTimeout(t)}}var Kp=fk;var lO={eq:function(t){return function(r){return t===r}}},Yp={compare:function(t){return function(r){return Kt(Le)(t)(r)}},Eq0:function(){return lO}};var i_=lk;var ac=function(r){return function(e){return r(e)()}};var xf=function(r){return function(e){return function(){return r(e)}}};var TO=function(t){return{append:function(r){return function(e){return ac(function(n){return Dt(Jm(t))(xf(r)(n))(xf(e)(n))})}}}};var wv=function(t){return{mempty:ac(function(r){return vr(Vi(t))}),Semigroup0:function(){return TO(t.Semigroup0())}}};var No=function(t){return t.sampleOnRight};var Ce=function(t){return t.keepLatest};var Ff=function(t){return t.fix},Ru=function(t){return function(r){return function(e){return function(n){return Ff(t)(function(a){return No(t)(X(t.Alternative0().Plus1().Alt0())(a)(l(t.Alternative0().Applicative0())(e)))(m(t.Filterable1().Functor1())(yr(r))(n))})}}}};var Of=function(t){return function(r){return function(e){return function(n){return bi(t.Filterable1())(Qe)(Ru(t)(function(a){return function(u){return m(xo)(l(To))(r(a.value0)(u))}})(new rt(e,w.value))(n))}}}},ts=function(t){return function(r){var e=function(n){return function(a){if(n instanceof w)return new S({now:a,last:w.value});if(n instanceof S)return new S({now:a,last:new S(n.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 83, column 3 - line 83, column 50): "+[n.constructor.name,a.constructor.name])}};return bi(t.Filterable1())(nt(at))(Ru(t)(e)(w.value)(r))}};function Mv(t){return function(r){return t===r}}var c_=Mv;var vk=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var $O=function(t){return function(r){return function(e,n){var a=Br(w.value)(),u=t(e,function(i){return ne(new S(i))(a)()}),o=r(e,function(i){var f=Jr(a)();return pn(vt)(Zr)(f)(function(p){return function(){return n(i(p))}})()});return function(){return u(),o()}}}},wO=function(t){return function(r){return function(e,n){var a=Br(w.value)(),u=t(e,function(i){var f=Jr(a)();return pn(vt)(Zr)(f)(function(p){return function(){return n(p(i))}})()}),o=r(e,function(i){return ne(new S(i))(a)()});return function(){return u(),o()}}}},MO=function(t){return function(r,e){var n=Br(l(vt)(void 0))(),a=t(r,function(u){var o=Jr(n)();o();var i=u(r,e);return ne(i)(n)()});return function(){var o=Jr(n)();return o(),a()}}},g={map:function(t){return function(r){return function(e,n){return r(e,function(a){return n(t(a))})}}}},f_=function(t){return function(r){return function(e,n){return r(e,function(a){var u=t(a);if(u instanceof S)return n(u.value0);if(u instanceof w)return void 0;throw new Error("Failed pattern match at FRP.Event (line 190, column 31 - line 192, column 35): "+[u.constructor.name])})}}},Pv=function(t){return f_(function(r){var e=t(r);if(e)return new S(r);if(!e)return w.value;throw new Error("Failed pattern match at FRP.Event (line 117, column 13 - line 119, column 25): "+[e.constructor.name])})},Dk=function(){var r=Br([])();return{event:function(e,n){var a=Br(n)();return _i(function(u){return Dt(Je)(u)([a])})(r)(),function(){return ne(vr(wv(zi)))(a)(),li(Kl(c_)(a))(r)(),void 0}},push:function(e){var n=Jr(r)();return Zu(n)(function(a){return function(){var o=Jr(a)();return o(e)}})()}}},PO=function(t){return function(r,e){var n=Dk(),a=t(n.event),u=a(r,n.push),o=n.event(r,e);return function(){return u(),o()}}},dk={compact:f_(nt(at)),separate:function(t){return{left:f_(function(r){if(r instanceof tr)return new S(r.value0);if(r instanceof rr)return w.value;throw new Error("Failed pattern match at FRP.Event (line 100, column 13 - line 102, column 33): "+[r.constructor.name])})(t),right:f_(function(r){if(r instanceof rr)return new S(r.value0);if(r instanceof tr)return w.value;throw new Error("Failed pattern match at FRP.Event (line 107, column 13 - line 109, column 32): "+[r.constructor.name])})(t)}}},ru={filter:Pv,filterMap:f_,partition:function(t){return function(r){return{yes:Pv(t)(r),no:Pv(function(){var e=Iu(bu);return function(n){return e(t(n))}}())(r)}}},partitionMap:function(t){return function(r){return{left:bi(ru)(function(){var e=Wa(S.create)(E(w.value));return function(n){return e(t(n))}}())(r),right:bi(ru)(function(e){return Tp(t(e))})(r)}}},Compactable0:function(){return dk},Functor1:function(){return g}},IO=function(t){return function(r){return function(e,n){var a=Br(w.value)(),u=ea(ra)(Pc)(),o=Br(w.value)(),i=ea(ra)(Pc)(),f=Br(!0)(),p=t(e,function(c){var D=Jr(f)();if(D)return ir(N)(ea(ra)(Gi(c)(u)))();ne(new S(c))(a)();var M=Jr(o)();return pn(vt)(Zr)(M)(function(et){return function(){return n(et(c))}})()}),_=r(e,function(c){var D=Jr(f)();if(D)return ir(N)(ea(ra)(Gi(c)(i)))();ne(new S(c))(o)();var M=Jr(a)();return pn(vt)(Zr)(M)(function(et){return function(){return n(c(et))}})()});ne(!1)(f)();var s=ea(ra)(Mp(u))(),v=ea(ra)(Mp(i))();return function(){return s.length===0?ne(dv(v))(o)():Zu(s)(function(c){return function(){return ne(new S(c))(a)(),Zu(v)(function(M){return function(){return ne(new S(M))(o)(),n(M(c))}})()}})()}(),ea(ra)(wp(0)(la(s))([])(u))(),ea(ra)(wp(0)(la(v))([])(i))(),function(){return p(),_()}}}},Be=function(t){return function(r){return r}(Rv(311).subscribe)(t)},Rv=vk("backdoor","FRP.Event",function(){var t=function(){var e=Br([])();return{event:function(n,a){var u=Br(a)();return _i(function(o){return Dt(Je)(o)([u])})(e)(),function(){return ne(vr(wv(zi)))(u)(),li(Kl(c_)(u))(e)(),void 0}},push:function(n){return function(){var u=Jr(e)();return Zu(u)(function(o){return function(){var f=Jr(o)();return f(n)}})()}}}};return{makeEvent:function(){var r=function(e){return function(n,a){return n?l(vt)(void 0):e(function(u){return function(){return a(u)}})()}};return r}(),makePureEvent:function(){var r=function(e){return function(n,a){return e(function(u){return function(){return a(u)}})()}};return r}(),makeLemmingEvent:function(){var r=function(e){return function(n,a){var u=function(o){return function(i){return function(){return o(n,ac(i))}}};return e(u)(function(o){return function(){return a(o)}})()}};return r}(),create:t,createPure:t,subscribe:function(){var r=function(e){return function(n){return function(){return e(!1,ac(n))}}};return r}(),subscribePure:function(){var r=function(){var e=function(n){return function(a){return function(){return n(!0,ac(a))}}};return e}();return r}(),bus:function(){var r=function(e){return function(n,a){var u=Iv(609)();return a(e(u.push)(u.event)),l(vt)(void 0)}};return r}(),memoize:function(){var r=function(e){return function(n){return function(a,u){var o=Dk();return u(n(o.event)),e(a,o.push)}}};return r}(),hot:function(){var r=function(e){return function(){var a=Iv(627)(),u=Be(e)(a.push)();return{event:a.event,unsubscribe:u}}};return r}(),mailboxed:function(){var r=function(e){return function(n){return function(a){return function(u,o){var i=Br(rc)();o(a(function(p){return function(_,s){return ir(N)(li(ec(e)(function(v){if(v instanceof w)return new S([s]);if(v instanceof S)return new S(Dt(Je)(v.value0)([s]));throw new Error("Failed pattern match at FRP.Event (line 640, column 21 - line 642, column 55): "+[v.constructor.name])})(p))(i))(),ir(N)(li(ec(e)(function(v){if(v instanceof w)return w.value;if(v instanceof S)return new S(Kl(c_)(s)(v.value0));throw new Error("Failed pattern match at FRP.Event (line 649, column 21 - line 651, column 69): "+[v.constructor.name])})(p))(i))}}));var f=n(u,function(p){var _=Jr(i)(),s=Qp(e)(p.address)(_);if(s instanceof w)return void 0;if(s instanceof S)return Zu(s.value0)(function(v){return function(){return v(p.payload)}})();throw new Error("Failed pattern match at FRP.Event (line 658, column 13 - line 660, column 70): "+[s.constructor.name])});return function(){return ir(N)(ne(rc)(i))(),f()}}}}};return r}(),delay:function(){var r=function(e){return function(n){return function(a,u){var o=Br(vr(ck(Yp)))(),i=n(a,function(f){var p=Br(w.value)(),_=Kp(e)(function(){u(f);var v=Jr(p)();return Ut(l(vt)(void 0))(function(c){return _i(Xp(Yp)(c))(o)})(v)()})();return ne(new S(_))(p)(),_i(Dt(Fv(Yp))(ak(_)))(o)()});return function(){var p=Jr(o)();return pn(vt)(ik)(p)(i_)(),i()}}}};return r}()}}),Iv=vk("create","FRP.Event",function(){return function(){return void 0,function(r){return r}(Rv(388).create)()}}),l_=Rv(520),bk=Iv(385),Nv=function(t){return function(r){return r}(l_.bus)(t)};var Nu=function(t){return function(r){return r}(l_.delay)(t)};var xa=function(t){return function(r){return r}(l_.makeEvent)(t)},Sr=function(t){return function(r){return r}(l_.makeLemmingEvent)(t)};var oo=function(t){return function(r){return r}(l_.memoize)(t)};var mn={apply:function(t){return function(r){return IO(t)(m(g)(Ec)(r))}},Functor0:function(){return g}};var C={pure:function(t){return function(r,e){return e(t),l(vt)(void 0)}},Apply0:function(){return mn}};var K={alt:function(t){return function(r){return function(e,n){return yt(kt)(m(N)(function(a){return function(u){return function(){return a(),u()}}})(function(){return t(e,n)}))(function(){return r(e,n)})()}}},Functor0:function(){return g}};var A={empty:function(t,r){return l(vt)(void 0)},Alt0:function(){return K}},RO={Applicative0:function(){return C},Plus1:function(){return A}},Tr={keepLatest:MO,sampleOnRight:$O,sampleOnLeft:wO,fix:PO,Alternative0:function(){return RO},Filterable1:function(){return ru}};var NO=function(t){return t},__=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),p_=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),LO=function(t){return t},rs=O(),b=LO;var B=function(){return __.create}();var ft=function(){return p_.create}(),te=function(){var t=m(Qa)(m(N)(E(!0)));return function(r){return NO(t(r))}}(),tt=function(t){return t.attr};function yk(t){return()=>t.slice()}function Ak(t){return r=>e=>()=>{e[t]=r}}function kk(t){return()=>t.slice()}function s_(t,r){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);for(var a in t)({}).hasOwnProperty.call(t,a)&&(e[a]=t[a]);return e}var Ck=function(t){return function(){return function(){return function(r){return function(e){return function(n){return Qu(ke(t)(r))(e)(n)}}}}}};var hk=function(){return function(){return function(t){return function(r){return s_(t,r)}}}},Uc=function(t){return function(){return function(){return function(r){return function(e){return function(n){return Qu(ke(t)(r))(e)(n)}}}}}},Bo=function(t){return function(){return function(r){return function(e){return Xa(ke(t)(r))(e)}}}};var Nn={vb:function(t){return l(_r)(new rt({},{}))}},es=function(t){return t.vb},zO={vbus:function(){var t=function(){return function(e){return function(n){return function(a){return Sr(function(u){return function(o){return function(){var f=es(e)(d.value)();return o(a(f.value0)(f.value1))(),l(_r)(void 0)}}})}}}},r=function(){return function(e){return t()(e)}};return r}()},Bv=function(){return function(t){return function(r){return function(e){return e()(t)}(zO.vbus)(r)}}},Lu=function(t){return function(){return function(){return function(){return function(r){return function(e){return function(){return function(){return{vb:function(n){return function(){var u=es(e)(d.value)(),o=es(r)(d.value)();return new rt(Uc(t)()()(d.value)(o.value0)(u.value0),Uc(t)()()(d.value)(o.value1)(u.value1))}}}}}}}}}}},oe=function(t){return function(){return function(){return function(r){return function(){return function(){return{vb:function(e){return function(){var a=es(r)(d.value)(),u=bk();return new rt(Uc(t)()()(d.value)(u.push)(a.value0),Uc(t)()()(d.value)(u.event)(a.value1))}}}}}}}}};var Wo=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),m_=function(){function t(){}return t.value=new t,t}();var $f=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),qc=function(){function t(){}return t.value=new t,t}(),Wv=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Uv=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),v_=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),qo=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),F=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Ek=function(t){return t};var Hc={eq:function(t){return function(r){return t instanceof Wo&&r instanceof Wo?t.value0===r.value0:t instanceof m_&&r instanceof m_}}};var L=function(t){return new v_(t)},D_=function(t){return new qo(t)},d_=function(t){return new Uv(t)};var Sk=t=>r=>r[t];var Ho=function(t){return t.reflectType};var xk=function(t){return Ho(t)};var b_=Or;var zc=function(){return function(t){return t}};var qv=function(t){return[t]};var Hv=function(){return function(){return function(){return function(){return function(){return function(t){var r=xk(t);return function(e){return Sk(r(e))}}}}}}};var zv=[];var Vc=function(){return function(){return function(t){return function(r){return Xi(t)(r)}}}};function wk(t){return function(){var r={};for(var e in t)hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}}var zo={};function Vv(t){return t()}function Mk(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(t[n]));return e}function Pk(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(n)(t[n]));return e}function Ik(t){return function(r){return function(e){return function(n){var a=e;function u(i){return function(f){return r(f)(i)(n[i])}}for(var o in n)hasOwnProperty.call(n,o)&&(a=t(a)(u(o)));return a}}}}function y_(t){return function(r){var e=[];for(var n in r)hasOwnProperty.call(r,n)&&e.push(t(n)(r[n]));return e}}var QO=Object.keys||y_(function(t){return function(){return t}});function Gv(t){return function(r){return function(e){return function(){return e[t]=r,e}}}}var Jv=function(t){return function(r){return function(){return delete r[t],r}}};var jv=y_(function(t){return function(r){return r}});var e$=wk;var Nk=function(t){return function(r){return Vv(function(){var n=e$(r)();return t(n)(),n})}};var Lk=function(t){return function(r){return Pk(r,t)}};var Ai=function(t){return function(r){return Nk(Gv(t)(r))}},us={map:function(t){return function(r){return Mk(r,t)}}},n$={mapWithIndex:Lk,Functor0:function(){return us}},Qv=function(){return pt};var os=Ik(Ec),Bk=function(t){return function(r){return os(function(e){return function(n){return function(a){return Dt(t.Semigroup0())(e)(r(n)(a))}}})(vr(t))}},A_={foldl:function(t){return os(function(r){return function(e){return t(r)}})},foldr:function(t){return function(r){return function(e){return ue(_t)(t)(r)(jv(e))}}},foldMap:function(t){return function(r){return Bk(t)(E(r))}}},Wk={foldlWithIndex:function(t){return os(yr(t))},foldrWithIndex:function(t){return function(r){return function(e){return ue(_t)(Af(t))(r)(y_(rt.create)(e))}}},foldMapWithIndex:function(t){return Bk(t)},Foldable0:function(){return A_}},a$={traverseWithIndex:function(t){return function(r){return function(e){return os(function(n){return function(a){return function(u){return yt(t.Apply0())(m(t.Apply0().Functor0())(yr(Ai(a)))(n))(r(a)(u))}}})(l(t)(zo))(e)}}},FunctorWithIndex0:function(){return n$},FoldableWithIndex1:function(){return Wk},Traversable2:function(){return wf}},wf={traverse:function(t){var r=Ki(a$)(t);return function(e){return r(E(e))}},sequence:function(t){return Rn(wf)(t)(nt(at))},Functor0:function(){return us},Foldable1:function(){return A_}};var Xv=function(t){return Nk(Jv(t))};function Uk(t){var r={};for(var e in t)({}).hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}function qk(t){return function(r){return function(e){return e[t]=r,e}}}function Hk(t){return function(r){return function(e){return e[t]=r(e[t]),e}}}function zk(t){return function(r){return delete r[t],r}}var Mf=ii;var Vk=function(){return function(){return function(t){return function(r){return function(e){return function(n){return Hk(ke(t)(r))(e)(n)}}}}}};var k_=function(){return function(){return function(t){return function(r){return function(e){return function(n){return qk(ke(t)(r))(e)(n)}}}}}};var Gk=function(t){return function(){return function(){return function(r){return function(e){return zk(ke(t)(r))(e)}}}}},Kv=at,is=function(t){return function(r){return t(Uk(r))}},Jk=yr(is)({});var jk=function(){function t(){}return t.value=new t,t}(),Zv=function(){function t(){}return t.value=new t,t}(),o$=function(){function t(){}return t.value=new t,t}();var eu=function(t){return function(r){return function(e){var n=function(a){return a(r)(e)};return function(a){if(a instanceof v_)return _n(_t)(A)(eu(t)(r)(e))(a.value0);if(a instanceof qo)return Ce(Tr)(m(g)(eu(t)(r)(e))(a.value0));if(a instanceof F)return n(t.toElt(a.value0));if(a instanceof Uv)return Sr(function(u){return function(o){return function(){var f=je(zo)(),p=u(a.value0)(function(_){return function(){var v=t.ids(e)(),c=je(l(_r)(void 0))(),D=t.ids(e)(),M=je(l(_r)(void 0))(),et=je([])(),Ft=je(l(_r)(void 0))(),Wt=m(xe)(Wo.create)(t.ids(e))(),Xr=je(jk.value)(),ve=u(_)(function(zt){return function(){var Pt=Tn(Xr)();if(zt instanceof Wv&&Pt instanceof Zv)return mt(Mc)(Tn(et))(Ie(_r)(_t)(function(){var qe=t.doLogic(zt.value0)(e);return function(Ea){return o(qe(Ea))}}()))();if(zt instanceof qc&&Pt instanceof Zv){ir(xe)(du(o$.value)(Xr))();var le=function(){var Ea=Tn(et)();pn(_r)(_t)(Ea)(function(Gu){return pn(_r)(Zr)(r.parent)(function(ui){return o(t.disconnectElement(e)({id:Gu,parent:ui,scope:Wt}))})})();var ca=Tn(c)();ca();var He=Tn(M)();return He(),ir(xe)(eo(Xv(v))(f))(),ir(xe)(eo(Xv(D))(f))()};return ir(xe)(du(le)(Ft))(),le()}if(zt instanceof $f&&Pt instanceof jk){ir(xe)(du(Zv.value)(Xr))();var Vn=u(eu(t)(function(){var qe={};for(var Ea in r)({}).hasOwnProperty.call(r,Ea)&&(qe[Ea]=r[Ea]);return qe.scope=Wt,qe.raiseId=function(ca){return ir(xe)(eo(Dt(Je)([ca]))(et))},qe}())(e)(zt.value0))(o)();return ir(xe)(eo(Ai(D)(Vn))(f))(),ir(xe)(du(Vn)(M))()}return void 0}})();ir(xe)(du(ve)(c))(),ir(xe)(eo(Ai(v)(ve))(f))();var lt=Tn(Ft)();return lt()}})();return function(){return mt(Mc)(Tn(f))(pe(A_)(dt($p))(l(_r)(void 0)))(),p()}}}});throw new Error("Failed pattern match at Bolson.Control (line 543, column 17 - line 628, column 20): "+[a.constructor.name])}}}},i$=function(){return function(){return function(){return function(t){return function(r){return function(e){return function(n){return function(a){return function(u){var o=function(i){return function(f){return Sr(function(p){return function(_){return function(){var v=yk(m(Or)(E(""))(zc()(a)))(),c=Yt(_t)(A)($o(mi)(function(ve){return Bm(gy)(function(lt){return function(zt){return zt instanceof F?function(Fr){return Fr(function(){var Pt={};for(var le in i)({}).hasOwnProperty.call(i,le)&&(Pt[le]=i[le]);return Pt.parent=w.value,Pt.scope=r(i.scope),Pt.raiseId=function(Vn){return Ak(ve)(Vn)(v)},Pt}())(f)}(n.toElt(zt.value0)):lt(n.wrapElt(zt))}})})(zc()(a))),D=p(c)(_)(),M=je(l(_r)(void 0))(),et=m(xe)(pt)(kk(v))(),Ft=m(b_)(function(ve){return function(lt){return new F(n.fromEltO1(function(zt){return function(Fr){return Sr(function(Pt){return function(le){return function(){return zt.raiseId(ve)(),pn(_r)(Zr)(zt.parent)(function(qe){return le(n.giveNewParent(Fr)(is(pf(Mf)(k_()()({reflectSymbol:function(){return"id"}})(d.value)(ve))(pf(Mf)(Vk()()({reflectSymbol:function(){return"parent"}})(d.value)(E(qe)))(Gk({reflectSymbol:function(){return"raiseId"}})()()(d.value))))(zt))(lt))})(),l(_r)(void 0)}}})}}))}})(et),Wt=eu(e)(i)(f)(u(Ft)(pt)),Xr=p(Wt)(_)();return ir(xe)(du(Xr)(M))(),function(){D(),Pn(_r)(!t)(pn(_r)(_t)(zc()(et))(function(zt){return _(n.deleteFromCache(f)({id:zt}))}))();var lt=Tn(M)();return lt()}}}})}};return new F(n.fromEltO2(o))}}}}}}}}};var tD=function(){return function(){return function(){return function(t){return function(r){return function(e){return function(n){return i$()()()(!1)(nt(at))(t)(r)(e)(n)}}}}}}};var Qk=function(t){return function(r){return function(e){var n=function(a){return function(u){return Sr(function(o){return function(i){return function(){var p=je(w.value)(),_=e(new F(r.fromElt(function(s){return function(v){return Sr(function(c){return function(D){return function(){return function(){var Ft=Tn(p)();if(Ft instanceof w)return void 0;if(Ft instanceof S)return pn(_r)(Zr)(s.parent)(function(Wt){return Pn(_r)(Ft.value0!==Wt)(dt($p)(s.raiseId(Ft.value0))(D(r.connectToParent(u)({id:Ft.value0,parent:Wt}))))})();throw new Error("Failed pattern match at Bolson.Control (line 653, column 27 - line 660, column 16): "+[Ft.constructor.name])}(),l(_r)(void 0)}}})}})));return o(eu(t)(function(){var s={};for(var v in a)({}).hasOwnProperty.call(a,v)&&(s[v]=a[v]);return s.parent=a.parent,s.scope=a.scope,s.raiseId=function(c){return function(){return a.raiseId(c)(),ir(xe)(du(new S(c))(p))()}},s}())(u)(_))(i)()}}})}};return new F(r.fromElt(n))}}};var xn={dimap:function(t){return function(r){return function(e){return function(n){return r(e(t(n)))}}}}},Vo=function(t){return t.dimap},Fa=function(t){return function(r){return Vo(t)(r)(nt(at))}};var Wu=function(){return function(t){return function(r){return function(e){return new qo(O()(Bv()(t)(r)(e)))}}}};var Xk=function(t){return function(r){var e=function(n){return n instanceof F?new F(Fa(xn)(function(a){return{pos:t,dynFamily:a.dynFamily,ez:a.ez,parent:a.parent,raiseId:a.raiseId,scope:a.scope}})(n.value0)):n instanceof qo?new qo(m(g)(e)(n.value0)):r};return e(r)}},j=function(t){return Xk(S.create(t))};var c$=function(){return{doLogic:function(t){return function(r){return function(e){return r.sendToPos({id:e,pos:t})}}},ids:function(){var t=ae();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:Vt(Hc)})}},toElt:function(t){return t}}},Kk=function(t){return new $f(ae()(Xk(w.value)(t)))};var rD=function(t){return Nv(t)};var Ke=function(t){return new qo(O()(rD(t)))},Yk=function(t){return Ke(Ic(t))},f$=function(t){return function(r){return function(e){return eu(c$())(t)(r)(e)}}},l$=function(t){return function(r){var e=function(n){return function(a){return Sr(function(u){return function(o){return function(){var f=a.ids();n.raiseId(f)();var p=function(){if(n.parent instanceof w){var s=a.ids();return new rt(l(C)(a.makeElement({id:s,parent:w.value,scope:n.scope,tag:"div",pos:w.value,dynFamily:w.value})),s)}if(n.parent instanceof S)return new rt(T(A),n.parent.value0);throw new Error("Failed pattern match at Deku.Core (line 338, column 34 - line 352, column 36): "+[n.parent.constructor.name])}(),_=u(Yt(_t)(A)([p.value0,l(C)(a.makeDynBeacon({id:f,parent:new S(p.value1),scope:n.scope,dynFamily:n.dynFamily,pos:n.pos})),l(C)(a.attributeParent({id:f,parent:p.value1,pos:n.pos,dynFamily:n.dynFamily,ez:n.ez})),f$({parent:new S(p.value1),scope:n.scope,ez:!1,raiseId:function(s){return l(_r)(void 0)},pos:w.value,dynFamily:new S(f)})(a)(t(r))]))(o)();return function(){return o(a.removeDynBeacon({id:f}))(),_()}}}})}};return new F(e)}},Zk=l$(O()(d_));var s$=function(t){return function(r){return function(e){return m(g)(function(n){return t.setText(function(a){return{id:r,text:a}}(n))})(e)}}},m$=function(t){return function(r){return function(e){return m(g)(function(n){return function(a){if(a.value instanceof __)return t.setProp({id:r,key:a.key,value:a.value.value0});if(a.value instanceof p_)return t.setCb({id:r,key:a.key,value:a.value.value0});throw new Error("Failed pattern match at Deku.Control (line 61, column 26 - line 63, column 45): "+[a.value.constructor.name])}(rs(n))})(e)}}},un=function(t){var r=function(e){return function(n){return Sr(function(a){return function(u){return function(){var i=n.ids();e.raiseId(i)();var f=a(Yt(_t)(A)([l(C)(n.makeText({id:i,parent:e.parent,pos:e.pos,scope:e.scope,dynFamily:e.dynFamily})),s$(n)(i)(t),Ut(T(A))(function(p){return l(C)(n.attributeParent({id:i,parent:p,pos:e.pos,dynFamily:e.dynFamily,ez:e.ez}))})(e.parent)]))(u)();return function(){return u(n.deleteFromCache({id:i}))(),f()}}}})}};return new F(r)},ie=function(t){return un(l(C)(t))},tg=function(t){return function(r){var e=function(){var n=function(a){return function(u){return new rt(a+1|0,new rt(u,a))}};return Of(Tr)(n)(0)}();return Zk(Ce(Tr)(oo(e(r))(function(n){return m(g)(function(a){return Yt(_t)(A)([m(g)(E(qc.value))(Tf(ru)(function(){var u=Vt(ci)(a.value1+1|0);return function(o){return u(Qe(o))}}())(n)),l(C)(Kk(O()(t(a.value0))))])})(n)})))}},v$=function(){return{doLogic:function(t){return function(r){return function(e){return r.sendToPos({id:e,pos:t})}}},ids:function(){var t=ae();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:Vt(Hc)})}},toElt:function(t){return t}}};var rg=function(t){return function(r){return function(e){return eu(v$())(t)(r)(e)}}},eg=function(t){return function(r){return function(e){return Sr(function(n){return function(a){return n(X(K)(l(C)(e.makeRoot({id:"deku-root",root:t})))(rg({parent:new S("deku-root"),scope:new Wo("rootScope"),raiseId:function(u){return l(_r)(void 0)},ez:!0,pos:w.value,dynFamily:w.value})(e)(r)))(a)}})}}},Q=function(t){return function(r){return function(e){var n=function(a){return function(u){return Sr(function(o){return function(i){return function(){var p=u.ids();a.raiseId(p)();var _=o(X(K)(Yt(_t)(A)(Dt(Je)([l(C)(u.makeElement({id:p,parent:a.parent,scope:a.scope,tag:t,pos:a.pos,dynFamily:a.dynFamily})),m$(u)(p)(r)])(Ut([])(function(s){return[l(C)(u.attributeParent({id:p,parent:s,pos:a.pos,dynFamily:a.dynFamily,ez:a.ez}))]})(a.parent))))(rg({parent:new S(p),scope:a.scope,ez:!0,raiseId:function(s){return l(_r)(void 0)},pos:w.value,dynFamily:w.value})(u)(e)))(i)();return function(){return i(u.deleteFromCache({id:p}))(),_()}}}})}};return n}}};var se=function(){function t(){}return t.value=new t,t}();var be={attr:function(t){return function(r){return b({key:"click",value:ft(r)})}}};var Jt=function(){function t(){}return t.value=new t,t}();var cs={attr:function(t){return function(r){return b({key:"style",value:B(r)})}}};var ng={attr:function(t){return function(r){return b({key:"style",value:B(r)})}}};var bt={attr:function(t){return function(r){return b({key:"style",value:B(r)})}}};var ag={attr:function(t){return function(r){return b({key:"style",value:B(r)})}}},Pf={attr:function(t){return function(r){return b({key:"style",value:B(r)})}}};var eD={attr:function(t){return function(r){return b({key:"style",value:B(r)})}}};var ug={attr:function(t){return function(r){return b({key:"style",value:B(r)})}}};var nD=function(t){return function(r){return new F(Q("a")(t)(O()(L(O()(G(j)(r))))))}};var xr=function(t){return function(r){return new F(Q("div")(t)(O()(L(O()(G(j)(r))))))}},zr=xr(T(A));var Rf=function(t){return function(r){return new F(Q("span")(t)(O()(L(O()(G(j)(r))))))}},aD=Rf(T(A));var d$=function(t){return function(r){return Yk(function(e){return D_(O()(oo(t(e.value1))(function(n){return r(new rt(e.value0,n))})))})}},ig=function(t){return d$(function(r){return X(K)(l(C)(t))(r)})};var cg=function(t){return function(r){return t(r)}};var fg=t=>r=>e=>()=>{if(e.units[r.id]){let n=e.units[r.parent].main;e.units[r.id].main&&e.units[r.id].main.parentNode||e.units[r.id].startBeacon&&e.units[r.id].startBeacon.parentNode||(r.ez?(()=>(e.units[r.id].main?n.appendChild(e.units[r.id].main):(n.appendChild(e.units[r.id].startBeacon),n.appendChild(e.units[r.id].endBeacon)),!0))():t(r.pos)(u=>()=>t(r.dynFamily)(o=>()=>{for(var i=0,f=0,p;f<n.childNodes.length;){if(n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue==="%-%"+o){f+=1;break}f++}let _=v=>{e.units[r.id].startBeacon?(n.insertBefore(e.units[r.id].startBeacon,n.childNodes[v]),n.insertBefore(e.units[r.id].endBeacon,n.childNodes[v])):n.insertBefore(e.units[r.id].main,n.childNodes[v])};for(;f<n.childNodes.length;){var s;if((s=n.childNodes[f].$dekuId)&&t(e.units[s].dynFamily)(c=>()=>t(e.units[s].pos)(M=>()=>o===c&&u<=M?(_(f),!0):!1)())())return!0;if(i===u||n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue==="%-%"+o+"%-%")return _(f),!0;n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue.substring(0,3)==="%-%"&&!p&&(p=n.childNodes[f].nodeValue+"%-%"),p||i++,n.childNodes[f].nodeType===8&&n.childNodes[f].nodeValue===p&&(p=void 0,i++),f++}return!1})())())||(r.parent.indexOf("@!%")!==-1?t(r.dynFamily)(o=>()=>(e.units[r.id].main?e.units[o].endBeacon.parentNode.insertBefore(e.units[r.id].main,e.units[o].endBeacon):(e.units[o].endBeacon.parentNode.insertBefore(e.units[r.id].endBeacon,e.units[o].endBeacon),e.units[o].endBeacon.parentNode.insertBefore(e.units[r.id].startBeacon,e.units[r.id].endBeacon)),!0))()||(e.units[r.id].main?n.parentNode.replaceChild(e.units[r.id].main,n):(n.parentNode.replaceChild(e.units[r.id].endBeacon,n),e.units[r.id].endBeacon.parentNode.insertBefore(e.units[r.id].startBeacon,e.units[r.id].endBeacon))):t(r.dynFamily)(o=>()=>(e.units[r.id].startBeacon?(n.insertBefore(e.units[r.id].startBeacon,e.units[o].endBeacon),n.insertBefore(e.units[r.id].endBeacon,e.units[o].endBeacon)):n.insertBefore(e.units[r.id].main,e.units[o].endBeacon),!0))()||(e.units[r.id].startBeacon?(n.appendChild(e.units[r.id].startBeacon),n.appendChild(e.units[r.id].endBeacon)):n.appendChild(e.units[r.id].main)))}};var lg=t=>r=>e=>n=>()=>{var a,u,o=e.id;if(n.scopes[e.scope]||(n.scopes[e.scope]=[]),n.scopes[e.scope].push(o),!t(e.parent)(()=>()=>n.hydrating&&r&&(a=n.allBeacons[e.id])&&(u=n.allBeacons[`${e.id}%-%`])?(n.units[o]={listeners:{},parent:e.parent,scope:e.scope,pos:e.pos,dynFamily:e.dynFamily,startBeacon:a,endBeacon:u},a.$dekuId=o,u.$dekuId=o,!0):!1)()){let f=document.createComment(`%-%${e.id}`),p=document.createComment(`%-%${e.id}%-%`);n.units[o]={listeners:{},parent:e.parent,dynFamily:e.dynFamily,scope:e.scope,pos:e.pos,startBeacon:f,endBeacon:p},f.$dekuId=o,p.$dekuId=o}};var _g=t=>r=>()=>r.units[t]&&r.units[t].dynFamily?r.units[t].dynFamily:(()=>{throw new Error(`No positional information for ${t}`)})(),pg=t=>r=>()=>r.units[t]&&r.units[t].main&&r.units[t].main.parentNode&&r.units[t].main.parentNode.$dekuId?r.units[t].main.parentNode.$dekuId:r.units[t]&&r.units[t].startBeacon&&r.units[t].startBeacon.parentNode&&r.units[t].startBeacon.parentNode.$dekuId?r.units[t].startBeacon.parentNode.$dekuId:(()=>{throw new Error(`No parent information for ${t}`)})(),sg=t=>r=>()=>r.units[t]&&r.units[t].scope?r.units[t].scope:(()=>{throw new Error(`No scope information for ${t}`)})(),mg=t=>r=>e=>n=>()=>{var a,u=e.id;if(n.scopes[e.scope]||(n.scopes[e.scope]=[]),n.scopes[e.scope].push(u),!t(e.parent)(()=>()=>n.hydrating&&r&&(a=document.documentElement.querySelector(`[data-deku-ssr="${u}"]`))?(n.units[u]={listeners:{},pos:e.pos,parent:e.parent,scope:e.scope,dynFamily:e.dynFamily,main:a},a.$dekuId=u,!0):!1)()){let i=document.createElement(e.tag);n.units[u]={listeners:{},parent:e.parent,pos:e.pos,scope:e.scope,dynFamily:e.dynFamily,main:i},i.$dekuId=u}},vg=t=>r=>e=>n=>a=>()=>{var u=n.id,o;if(a.scopes[n.scope]||(a.scopes[n.scope]=[]),a.scopes[n.scope].push(u),!t(n.parent)(f=>()=>{if(a.hydrating&&r&&(o=document.documentElement.querySelector(`[data-deku-ssr="${f}"]`))){for(var p=0;p<o.childNodes.length;p++){let c=u.split("@-@");if(o.childNodes[p].nodeType===8&&o.childNodes[p].nodeValue===c[0]){p=p-1;var _=p===-1,s=p>=0&&o.childNodes[p].nodeType===8;_&&o.prepend(document.createTextNode("")),s&&o.insertBefore(document.createTextNode(""),o.childNodes[p+1]);break}}let v=o.childNodes[p];return a.units[u]={main:v,pos:n.pos,parent:n.parent,scope:n.scope},v.$dekuId=u,!0}return!1})()){let f=document.createTextNode("");a.units[u]={main:f,parent:n.parent,scope:n.scope,pos:n.pos,dynFamily:n.dynFamily},f.$dekuId=u}};function uD(){return{units:{},scopes:{},allBeacons:{}}}var Dg=t=>r=>e=>()=>{if(e.units[r.id]){var n=r.id,a=r.value;e.hydrating&&t&&!e.units[n]&&(dom=document.documentElement.querySelector(`[data-deku-ssr="${n}"]`))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),e.units[n].main.tagName==="INPUT"&&r.key==="value"?e.units[n].main.value=a:e.units[n].main.tagName==="INPUT"&&r.key==="checked"?e.units[n].main.checked=a==="true":r.key==="disabled"?e.units[n].main.disabled=a==="true":e.units[n].main.setAttribute(r.key,a)}},dg=t=>r=>e=>()=>{if(e.units[r.id]){var n=r.id,a=r.value;if(e.hydrating&&t&&!e.units[n]&&(dom=document.documentElement.querySelector(`[data-deku-ssr="${n}"]`))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),r.key==="@self@")a(e.units[n].main)();else{e.units[n].listeners[r.key]&&e.units[n].main.removeEventListener(r.key,e.units[n].listeners[r.key]);var u=o=>a(o)();e.units[n].main.addEventListener(r.key,u),e.units[n].listeners[r.key]=u}}},bg=t=>r=>()=>{if(r.units[t.id]){var e=t.id;r.units[e].main.nodeValue=t.text}},yg=t=>r=>e=>n=>a=>()=>{var u,o,i=n.id,f=n.html,p=n.verb,_=n.cache,s=n.parent,v=n.scope,c=n.pxScope;let D=t(n.parent)(()=>()=>a.hydrating&&r&&(u=document.documentElement.querySelector(`[data-deku-ssr="${i}"]`))?(a.units[i]={listeners:{},pos:n.pos,scope:v,parent:s,main:u},u.$dekuId=i,!0):!1)();if(!D){let et=Object.entries(_);for(var M=0;M<et.length;M++){let Ft=et[M][0];et[M][1]===!0?f=f.replace(p+Ft+p,'data-deku-attr-internal="'+Ft+'"'):f=f.replace(p+Ft+p,'<span style="display:contents;" data-deku-elt-internal="'+Ft+'"></span>')}o=document.createElement("div"),o.innerHTML=f.trim(),a.units[i]={listeners:{},pos:n.pos,scope:v,parent:s,main:o.firstChild},o.firstChild.$dekuId=i}a.scopes[v]||(a.scopes[v]=[]),a.scopes[v].push(i),o||(o=u),o.querySelectorAll("[data-deku-attr-internal]").forEach(function(et){var Ft=et.getAttribute("data-deku-attr-internal");let Wt=Ft+"@!%"+c;a.units[Wt]={listeners:{},main:et,scope:v},a.scopes[v].push(Wt)}),o.querySelectorAll("[data-deku-elt-internal]").forEach(function(et){var Ft=et.getAttribute("data-deku-elt-internal");let Wt=Ft+"@!%"+c;a.units[Ft+"@!%"+c]={listeners:{},main:et,scope:v},a.scopes[v].push(Wt)}),D||a.units[i].main.remove()},Ag=t=>r=>()=>{var e=t.id;r.units[e]={main:t.root},t.root.$dekuId=e},oD=t=>r=>e=>n=>()=>{let a=(D,M,et)=>{if(n.units[D].startBeacon){var Ft=n.units[D].startBeacon,Wt=Ft.nextSibling;for(n.units[M].main.insertBefore(Ft,et),Ft=Wt;Ft&&Ft!==n.units[D].endBeacon;)Wt=Ft.nextSibling,n.units[M].main.insertBefore(Ft,et),Ft=Wt}else n.units[M].main.insertBefore(n.units[D].main,et)},u=[];u.push(e);for(var o=0;o<u.length;o++){let D=u[o],M=D.id,et=D.parent;n.units[M].containingScope=D.scope;var i=void 0;r(D.pos)(Wt=>()=>(i=Wt,!0))(),i===void 0&&(i=Number.MAX_VALUE);let Ft=n.units[et].main.childNodes;for(var f=0,p=!1,_=0;f<Ft.length;){var s;if(s=Ft[f].$dekuId){if(r(D.dynFamily)(Xr=>()=>p?!1:n.units[s].endBeacon===Ft[f]&&Xr===s?(n.units[M].pos=t(_),a(M,et,Ft[f]),!0):!1)()){p=!0;break}if(n.units[s].dynFamily!==n.units[M].dynFamily){f++;continue}if(p){f++;continue}_===i?(a(M,et,Ft[f]),_++,p=!0):n.units[s].endBeacon!==Ft[f]&&(n.units[s].pos=t(_),_++)}f++}if(p)return;if(n.units[M].main)n.units[et].main.appendChild(n.units[M].main);else{var v=n.units[M].startBeacon,c=v.nextSibling;for(n.units[et].main.appendChild(v),v=c;v&&v!==n.units[M].endBeacon;)c=v.nextSibling,n.units[et].main.appendChild(v),v=c}}},kg=t=>r=>()=>{if(r.units[t.id]){var e=t.id;if(r.units[e].containingScope&&!t.scopeEq(r.units[e].containingScope)(t.scope))return;if(r.units[e].main)r.units[e].main.remove();else{let u=document.createElement("div");var n=r.units[e].startBeacon,a=n.nextSibling;for(u.appendChild(n),n=a;n&&n!==r.units[e].endBeacon;)a=n.nextSibling,u.appendChild(n),n=a}}},iD=t=>r=>()=>{r.units[t.id]&&delete r.units[t.id]},gg=iD;var Cg=function(t){return function(r){return function(e){return(e|0)===e?t(e):r}}},jr=function(t){return t};var cD=function(t){return function(r){return Math.pow(t,r)|0}};var fs=isFinite;var C_=Math.floor;var oc=function(t){return function(r){return Math.pow(t,r)}},h_=function(t){return function(r){return t%r}},ls=Math.round;var _s=Math.sin;var ic=3.141592653589793;var Nf=function(){return Cg(S.create)(w.value)}(),Eg=function(t){if(!fs(t))return 0;if(t>=jr(Qn(Oc)))return Qn(Oc);if(t<=jr(Xn(Oc)))return Xn(Oc);if(re)return sa(0)(Nf(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},Sg=function(t){return Eg(ls(t))};var E_=function(t){return Eg(C_(t))};var Jo=Math.random;var S_=function(t){return function(r){return function(){var n=Jo(),a=(jr(r)-jr(t)+1)*n+jr(t);return E_(a)}}};var Tg=function(t){return t};var S$=1,ps=2147483647,T$=function(){return ps-1|0}(),cc=function(t){var r=function(e){return function(n){return function(a){var u=n-e|0,o=Za(Ku)(a)(u),i=o<e;return i?o+n|0:o}}};return r(S$)(T$)(t)};var x$=0,F$=48271,xg=function(t){return function(r){return Yn()(Nf(h_(jr(F$)*jr(r)+jr(t))(jr(ps))))}},Fg=xg(x$);var N$=function(){function t(o){this.fn=o}var r={},e=function(o,i){this.head=o,this.tail=i};function n(o){return new e(o,r)}function a(o){return function(i){return new e(o,i)}}function u(o){for(var i=[],f=o;f!==r;)i.push(f.head),f=f.tail;return i}return function(o){return function(i){return function(f){var p=function(s,v){return o(i(a)(f(s)))(v)},_=function(s,v,c){if(v===0)return s;var D=c[v-1];return new t(function(){var M=_(p(D,s),v-1,c);return M})};return function(s){for(var v=i(n)(f(s[s.length-1])),c=_(v,s.length-1,s);c instanceof t;)c=c.fn();return i(u)(c)}}}}}();var Pg=function(t){return t};var Ig=Je;var Rg=_t;var Bg=Pg,O_=function(t){return t};var $_=function(t){return Bg(MA(t))};var Lf=function(t){if(la(t)>0)return new S(Bg(t));if(re)return w.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 157, column 1 - line 157, column 58): "+[t.constructor.name])};var Wg=function(t){return function(r){return t(O_(r))}};var Ug=Wg(la);var qg=function(){return Wg(Dv())};var ki=function(t){return t.state};function jo(t){return new Error(t)}function M_(t){return function(){throw t}}function vs(t){return function(r){return function(){try{return r()}catch(e){return e instanceof Error||Object.prototype.toString.call(e)==="[object Error]"?t(e)():t(new Error(e.toString()))()}}}}var co=function(t){return t.throwError};var bw={throwError:M_,Monad0:function(){return fi}};var yD={catchError:yr(vs),MonadThrow0:function(){return bw}};var gi=function(t){return t.catchError};var P_=function(t){return function(r){return gi(t)(m(t.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(rr.create)(r))(function(){var e=l(t.MonadThrow0().Monad0().Applicative0());return function(n){return e(tr.create(n))}}())}};var ce={liftEffect:nt(at),Monad0:function(){return fi}},ye=function(t){return t.liftEffect};var ks=function(t){return{map:function(r){return function(e){return function(n){return m(t)(function(a){return new rt(r(a.value0),a.value1)})(e(n))}}}}};var AD=function(t){return{Applicative0:function(){return hs(t)},Bind1:function(){return gs(t)}}},gs=function(t){return{bind:function(r){return function(e){return function(n){return mt(t.Bind1())(r(n))(function(a){var u=e(a.value0);return u(a.value1)})}}},Apply0:function(){return Cs(t)}}},Cs=function(t){return{apply:su(AD(t)),Functor0:function(){return ks(t.Bind1().Apply0().Functor0())}}},hs=function(t){return{pure:function(r){return function(e){return l(t.Applicative0())(new rt(r,e))}},Apply0:function(){return Cs(t)}}};var kD=function(t){return{state:function(r){var e=l(t.Applicative0());return function(n){return e(r(n))}},Monad0:function(){return AD(t)}}};var Xg=function(t){return function(r){var e=t(r);return e.value0}};var Ew=function(t){return t};var Zg=function(){var t=function(r){return new rt(Tg(r.newSeed),function(){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);return e.newSeed=Fg(r.newSeed),e}())};return ki(kD(Pu))(t)}();var fo=ks(Xu),tC=m(fo)(function(t){return jr(t)/jr(ps)})(Zg);var pc=function(t){return Xg(Ew(t))};var Gc=gs(Pu);var Jc=Cs(Pu),Yg=function(t){return function(r){var e=jr(r),n=jr(t),a=function(i){return n+h_(i)(e-n+1)},u=m(fo)(jr)(Zg),o=yt(Jc)(m(fo)(Mr(Sa))(u))(m(fo)(Sn(Sa)(2))(u));return m(fo)(function(i){return E_(a(i))})(o)}},CD=function(t){return function(r){var e=t<=r;return e?Yg(t)(r):Yg(r)(t)}};var B_=hs(Pu);var hD=function(t){return mt(Gc)(CD(0)(Ug(t)-1|0))(function(r){return l(B_)(qg()(t)(r))})};var jc=function(t){return t.arbitrary};var rC={arbitrary:tC};var Es=function(){return{arbitrary:CD(-1e6)(1e6)}}();var Qc=function(t){return function(r){return t instanceof S?r(t.value0):l(vt)(!1)}},Mw=function(t){return function(r){return function(){var n=sg(t.id)(r)(),a=pg(t.id)(r)(),u=_g(t.id)(r)(),o={scope:n,parent:a,dynFamily:u,id:t.id,pos:new S(t.pos),ez:!1};return oD(S.create)(Qc)(o)(r)()}}};var eC=function(t){return{ids:function(){var e=Tn(t)(),n=Gt(Ba)(pc(jc(Es))({newSeed:cc(e),size:5}));return ir(xe)(eo(Mr(Ka)(1))(t))(),n},makeElement:mg(Qc)(!1),makeDynBeacon:lg(Qc)(!1),attributeParent:fg(Qc),makeRoot:Ag,makeText:vg(Qc)(!1)(Ut(void 0)),makePursx:yg(Qc)(!1)(Ut(void 0)),setProp:Dg(!1),setCb:dg(!1),setText:bg,sendToPos:Mw,removeDynBeacon:gg,deleteFromCache:iD,giveNewParent:oD(S.create)(Qc),disconnectElement:kg}};var Ci=function(){return window};function aC(t,r,e,n){if(typeof window<"u"){var a=window[e];if(a!=null&&n instanceof a)return r(n)}for(var u=n;u!=null;){var o=Object.getPrototypeOf(u),i=o.constructor.name;if(i===e)return r(n);if(i==="Object")return t;u=o}return t}var Nt=function(t){return function(r){return aC(w.value,S.create,t,r)}};function uC(t,r,e){return t==null?r:e(t)}var Ye=function(t){return uC(t,w.value,S.create)};var SD=Nt("HTMLCanvasElement");function cC(t){return function(){return t.body}}var fC=function(){var t=m(N)(Ye);return function(r){return t(cC(r))}}();var lC=pt;function Xc(t){return function(){return t.valueAsNumber}}var Hf=Nt("HTMLInputElement");function xD(t){return function(){return t.document}}function Fs(t){return function(r){return function(){return r.requestAnimationFrame(t)}}}var FD=pt;var CM=function(t){return function(r){return function(){var n=uD(),a=Gr(N)(ea(ra)(je(0)))(function(){var u=eg(t)(r);return function(o){return u(eC(o))}}())();return Be(a)(function(u){return u(n)})()}}},hM=function(t){return function(){var e=mt(Zn)(mt(Zn)(Ci)(xD))(fC)();return Ut(vr(Vi(Vi(zi))))(function(n){return CM(n)(t)})(m($e)(lC)(e))()}},sC=function(t){return ir(N)(hM(t))};var SM=function(t){return t};var Z={pursxToElement:function(t){return function(r){return function(e){return{cache:zo,element:function(n){return function(a){return T(A)}}}}}}},OD=function(t){return t.pursxToElement},on=function(){return function(t){return function(r){return function(e){return{pursxToElement:function(n){return function(a){return function(u){var o=OD(t)(n)(d.value)(u);return{cache:Ai(Ho(r)(d.value))(!0)(o.cache),element:function(i){return function(f){return X(K)(m(g)(Fa(xn)(rs)(function(p){if(p.value instanceof __)return f.setProp({id:Ho(r)(d.value)+("@!%"+n),key:p.key,value:p.value.value0});if(p.value instanceof p_)return f.setCb({id:Ho(r)(d.value)+("@!%"+n),key:p.key,value:p.value.value0});throw new Error("Failed pattern match at Deku.Pursx (line 4195, column 38 - line 4205, column 24): "+[p.value.constructor.name])}))(Bo(e)()(d.value)(u)))(o.element(i)(f))}}}}}}}}}}};var P=SM,pr=function(t){return function(r){return function(){return function(){return function(e){return function(n){return function(a){return function(u){var o=function(i){return function(f){return Sr(function(p){return function(_){return function(){var v=f.ids(),c=f.ids();i.raiseId(v)();var D=OD(e)(c)(d.value)(u),M=p(Yt(_t)(A)([l(C)(f.makePursx({id:v,parent:i.parent,cache:D.cache,dynFamily:i.dynFamily,pos:i.pos,pxScope:c,scope:i.scope,html:Ho(t)(a),verb:Ho(r)(n)})),D.element(i)(f),Ut(T(A))(function(et){return l(C)(f.attributeParent({id:v,parent:et,pos:i.pos,dynFamily:i.dynFamily,ez:!1}))})(i.parent)]))(_)();return function(){return _(f.deleteFromCache({id:v}))(),M()}}}})}};return new F(o)}}}}}}}},It=function(t){return function(){return function(){return function(r){return pr(t)({reflectType:function(){return"~"}})()()(r)(d.value)}}}};var TM=function(t){return function(r){return function(e){return eu({doLogic:function(n){return function(a){return function(u){return a.sendToPos({id:u,pos:n})}}},ids:function(){var n=ae();return function(a){return function(u){return u.ids}(n(a))}}(),disconnectElement:function(n){return function(a){return n.disconnectElement({id:a.id,scope:a.scope,parent:a.parent,scopeEq:Vt(Hc)})}},toElt:function(n){return n}})(t)(r)(O()(e))}}},I=function(){return function(t){return function(r){return function(e){return{pursxToElement:function(n){return function(a){return function(u){var o=Bo(e)()(d.value)(u),i=OD(t)(n)(d.value)(u);return{cache:Ai(Ho(r)(d.value))(!1)(i.cache),element:function(f){return function(p){return X(K)(TM({parent:new S(Ho(r)(d.value)+("@!%"+n)),scope:f.scope,raiseId:function(_){return l(_r)(void 0)},pos:f.pos,ez:!1,dynFamily:w.value})(p)(o))(i.element(f)(p))}}}}}}}}}}};var Ct=function(){return function(){return{defaults:yr(hk()())}}},xM=function(t){return t.defaults},ht={convertRecordOptions:function(t){return function(r){return function(e){return nt(Kv)}}}},mC=function(t){return t.convertRecordOptions},da=function(t){return t.convertOptionsWithDefaults},Et=function(){return function(t){return{convertOptions:function(r){return function(e){return Jk(mC(t)(r)(d.value)(e))}}}}},FM=function(t){return t.convertOptions},St=function(t){return function(r){return{convertOptionsWithDefaults:function(e){return function(n){var a=xM(r)(n),u=FM(t)(e);return function(o){return a(u(o))}}}}}},OM=function(t){return t.convertOption},z=function(t){return function(r){return function(){return function(){return function(){return function(e){return{convertRecordOptions:function(n){return function(a){return function(u){return Ju(Mf)(k_()()(e)(d.value)(OM(r)(n)(d.value)(Bo(e)()(d.value)(u))))(mC(t)(n)(d.value)(u))}}}}}}}}}};var wM=function(){return function(){return function(){return function(t){return function(r){return function(e){return Ep(e.type)(t)?Xa(e.type)(t)(e.value):r(e)}}}}}};var me=function(){return function(t){return function(r){return function(e){return{type:ke(t)(r),value:e}}}}};var MM=function(t){return Au("Data.Variant: pattern match failure ["+(t.type+"]"))},We=function(){return function(){return function(){return function(t){return wM()()()(t)(MM)}}}};var $D=function(){var t=zp(gv);return function(r){return Vp(t(r))}}();var Nut=typeof Array.from=="function",Lut=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",But=typeof String.prototype.fromCodePoint=="function",Wut=typeof String.prototype.codePointAt=="function";var hi={proof:function(t){return t},Coercible0:function(){}},MD=function(t){return t.proof};var Cu=void 0;var Ns=function(t){return t.toInt},kC=function(t){return function(r){return Ns(t)(Cu)}};var nu={toInt:function(t){return 8}},gC={Nat0:function(){return nu}},Xo={toInt:function(t){return 7}},CC={Nat0:function(){return Xo}},Ko={toInt:function(t){return 6}},hC={Nat0:function(){return Ko}},$a={toInt:function(t){return 5}},Ls={Nat0:function(){return $a}},Ln={toInt:function(t){return 4}},ua={Nat0:function(){return Ln}},Bn={toInt:function(t){return 3}},hu={Nat0:function(){return Bn}},Wn={toInt:function(t){return 2}},Eu={Nat0:function(){return Wn}},Un={toInt:function(t){return 1}},Su={Nat0:function(){return Un}},Oe={toInt:function(t){return 0}};var wr=function(t){return function(){return function(r){return function(){return function(e){return{Nat0:r.Nat1,Pos1:function(){return t}}}}}}};var lo={Nat0:function(){return Xo},Nat1:function(){return nu}};var _o={Nat0:function(){return Ko},Nat1:function(){return nu}};var po={Nat0:function(){return $a},Nat1:function(){return nu}};var so={Nat0:function(){return Ln},Nat1:function(){return nu}};var ba={Nat0:function(){return Ln},Nat1:function(){return $a}};var mo={Nat0:function(){return Bn},Nat1:function(){return nu}};var ya={Nat0:function(){return Bn},Nat1:function(){return $a}};var vo={Nat0:function(){return Wn},Nat1:function(){return nu}};var Aa={Nat0:function(){return Wn},Nat1:function(){return $a}};var Do={Nat0:function(){return Un},Nat1:function(){return nu}};var ka={Nat0:function(){return Un},Nat1:function(){return $a}};var bo={Nat0:function(){return Oe},Nat1:function(){return nu}};var ga={Nat0:function(){return Oe},Nat1:function(){return $a}};var EC={Nat0:function(){return Oe},Nat1:function(){return nu}};var PD={Nat0:function(){return Oe},Nat1:function(){return Xo}};var ID={Nat0:function(){return Oe},Nat1:function(){return Ko}};var U_={Nat0:function(){return Oe},Nat1:function(){return $a}};var Va={Nat0:function(){return Oe},Nat1:function(){return Ln}};var dn={Nat0:function(){return Oe},Nat1:function(){return Bn}};var bn={Nat0:function(){return Oe},Nat1:function(){return Wn}};var yn={Nat0:function(){return Oe},Nat1:function(){return Un}},Tu={Nat0:function(){return Oe},Nat1:function(){return Oe}};var SC=Oo;var Bs=function(t){return t};var q_=function(t){return function(){return function(r){return function(e){return r[Ns(t)(e)]}}}};var Ws=function(t){return function(r){var e=kC(t)(d.value),n=function(){return e===0?[]:en(0)(e-1|0)}();return m(Or)(r)(n)}};var qu=[];var Pr=function(t){return function(r){return function(e){return Xi(r)(e)}}};var An={first:function(t){return function(r){return new rt(t(r.value0),r.value1)}},second:m(xo),Profunctor0:function(){return xn}},qn=function(t){return t.second},Us=function(t){return t.first};var lP=function(t){return function(r){return function(e){return function(n){return Vo(e)(t)(r)(n)}}}};var OC=function(){return function(){return function(t){return lP(O())(O())(t)}}};var $C=function(){return function(){return function(t){return OC()()(t)}}};var sP=function(t){return function(r){return function(e){return Vo(r.Profunctor0())(t)(function(n){return n.value1(n.value0)})(Us(r)(e))}}},wC=function(t){return function(r){return function(e){return sP(function(n){return new rt(t(n),function(a){return r(n)(a)})})(e)}}};var MC=function(t){return function(){return function(){return function(r){return function(e){return wC(Bo(t)()(r))(yr(Ck(t)()()(r)))(e)}}}}};var PC=function(t){return t};var yP=JSON.parse;var AP=JSON.stringify;var qs=function(t){return t};var Hs=function(t){return t};var zs=function(t){return function(r){return t(r)}},H_=function(t){return{map:function(r){return zs(m(t)(m(wc)(r)))}}};var LD=function(t){return{Applicative0:function(){return z_(t)},Bind1:function(){return BD(t)}}},BD=function(t){return{bind:function(r){return function(e){return mt(t.Bind1())(r)(Wa(function(){var n=l(t.Applicative0());return function(a){return n(tr.create(a))}}())(function(n){var a=e(n);return a}))}},Apply0:function(){return RC(t)}}},RC=function(t){return{apply:su(LD(t)),Functor0:function(){return H_(t.Bind1().Apply0().Functor0())}}},z_=function(t){return{pure:function(){var r=l(t.Applicative0());return function(e){return qs(r(rr.create(e)))}}(),Apply0:function(){return RC(t)}}};var NC=function(t){return{throwError:function(){var r=l(t.Applicative0());return function(e){return qs(r(tr.create(e)))}}(),Monad0:function(){return LD(t)}}};var WD=function(t){return function(r){return{alt:function(e){return function(n){return mt(r.Bind1())(e)(function(a){if(a instanceof rr)return l(r.Applicative0())(new rr(a.value0));if(a instanceof tr)return mt(r.Bind1())(n)(function(u){if(u instanceof rr)return l(r.Applicative0())(new rr(u.value0));if(u instanceof tr)return l(r.Applicative0())(new tr(Dt(t)(a.value0)(u.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[u.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[a.constructor.name])})}},Functor0:function(){return H_(r.Bind1().Apply0().Functor0())}}}};var UD=function(){var t=ae();return function(r){return t(Hs(r))}}();function V_(t){return Object.prototype.toString.call(t).slice(8,-1)}var BC=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var zD=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var WC=pt;var VD=function(t){var r=co(NC(t));return function(e){return r($D(e))}};var GD=function(t){return function(r){return function(e){if(V_(e)===r)return l(z_(t))(WC(e));if(re)return VD(t)(new zD(r,V_(e)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[r.constructor.name,e.constructor.name])}}};var JD=function(t){return GD(t)("String")};var Js=function(){function t(){}return t.value=new t,t}(),js=function(){function t(){}return t.value=new t,t}(),HC=function(){function t(){}return t.value=new t,t}(),zC=function(){function t(){}return t.value=new t,t}(),QD=function(){function t(){}return t.value=new t,t}(),VC=function(){function t(){}return t.value=new t,t}(),GC=function(){function t(){}return t.value=new t,t}();var JC=function(t){return t},jC=function(t){return t};var QC=function(t){return t};var XC=function(t){return t};var KC=function(t){return t};var YC=function(t){return t},ZC=function(t){return t},th=function(t){return t},rh=function(t){return t},eh=function(t){return t};var XD=function(){function t(){}return t.value=new t,t}(),nh=function(){function t(){}return t.value=new t,t}(),ah=function(){function t(){}return t.value=new t,t}(),KD=function(){function t(){}return t.value=new t,t}(),uh=function(){function t(){}return t.value=new t,t}();var Qs=function(t){return t};var jf=function(t){return t};var LP=function(t){return t},G_=function(t){return t};var Zc={toAudioOnOff:nt(at)};var tf=function(t){return t.toAudioParameter},oh=function(t){return t.toAudioOnOff},ih=function(){return $f.create}(),ch=function(){return qc.value}();var Xs=function(){return PC(function(){var t=$C()()(xn),r=MC({reflectSymbol:function(){return"o"}})()()(d.value)(An);return function(e){return t(r(e))}}())},fh=pt;var BP=function(){var t=me()({reflectSymbol:function(){return"unit"}})(d.value);return function(r){return G_(t(r))}}();var WP=function(t){return function(r){return{toAudioParameter:function(e){return BP(e)}}}},lh=function(t){return function(r){return{toAudioParameter:function(){var e=tf(WP(t)(r));return function(n){return e(LP(function(a){return{u:a}}(n)))}}()}}},_h=function(){return me()({reflectSymbol:function(){return"2x"}})(d.value)(void 0)}(),ph=function(){var t=me()({reflectSymbol:function(){return"sudden"}})(d.value);return function(r){return G_(t(r))}}();var sh={toAudioParameter:ph},Ks={toAudioParameter:function(t){return ph({n:t})}},YD=function(){return me()({reflectSymbol:function(){return"step"}})(d.value)(void 0)}();var ZD=function(){return me()({reflectSymbol:function(){return"on"}})(d.value)(void 0)}(),J_={x:ZD,o:0},st=function(){return l(C)(Xe()(me()({reflectSymbol:function(){return"onOff"}})(d.value)(J_)))};var mh=function(){return me()({reflectSymbol:function(){return"off"}})(d.value)(void 0)}();var UP=function(){var t=me()({reflectSymbol:function(){return"numeric"}})(d.value);return function(r){return G_(t(r))}}();var Re={toAudioParameter:UP};var Yo=function(){return me()({reflectSymbol:function(){return"linear"}})(d.value)(void 0)}();var vh=function(){return me()({reflectSymbol:function(){return"exponential"}})(d.value)(void 0)}(),qP=function(){var t=me()({reflectSymbol:function(){return"envelope"}})(d.value);return function(r){return G_(t(r))}}();var On={toAudioParameter:qP},HP=function(){var t=me()({reflectSymbol:function(){return"cancel"}})(d.value);return function(r){return G_(t(r))}}();var Dh={toAudioParameter:HP};var zP=function(){function t(){}return t.value=new t,t}(),VP=function(){function t(){}return t.value=new t,t}(),GP=function(){function t(){}return t.value=new t,t}(),JP=function(){function t(){}return t.value=new t,t}(),jP=function(){function t(){}return t.value=new t,t}(),QP=function(){function t(){}return t.value=new t,t}(),XP=function(){function t(){}return t.value=new t,t}(),KP=function(){function t(){}return t.value=new t,t}(),YP=function(){function t(){}return t.value=new t,t}(),ZP=function(){function t(){}return t.value=new t,t}(),tI=function(){function t(){}return t.value=new t,t}(),rI=function(){function t(){}return t.value=new t,t}(),eI=function(){function t(){}return t.value=new t,t}(),nI=function(){function t(){}return t.value=new t,t}(),Ei=function(t){return{toPeriodicOscSpec:function(r){return me()({reflectSymbol:function(){return"realImg"}})(d.value)({real:Bs(r.value0),img:Bs(r.value1)})}}};var Ys={toInitializeTriangleOsc:function(t){return eh(function(r){return{frequency:r}}(t))}};var dh={toInitializeStereoPanner:function(t){return rh(function(r){return{pan:r}}(t))}};var Qf={toInitializeSquareOsc:function(t){return th(function(r){return{frequency:r}}(t))}};var sc={toInitializeSinOsc:function(t){return ZC(function(r){return{frequency:r}}(t))}};var bh={toInitializeSawtoothOsc:function(t){return YC(function(r){return{frequency:r}}(t))}};var td={toInitializeRecorder:function(t){return JC(function(r){return{cb:r}}(t))}};var j_={toInitializeMicrophone:function(t){return jC(function(r){return{microphone:r}}(t))}};var yh=function(t){return function(r){return{toInitializeIIRFilter:function(e){return function(n){return function(a){return{feedforward:MD(hi)(O()(e.value0)),feedback:MD(hi)(O()(e.value1))}}}}}}};var ct={toInitializeGain:function(t){return KC(function(r){return{gain:r}}(t))}};var Ah={toInitializeConvolver:function(t){return QC(function(r){return{buffer:r}}(t))}},Zs={toInitializeConstant:function(t){return XC(function(r){return{offset:r}}(t))}};var aI={convertOption:function(t){return function(r){return nt(at)}}},Q_={convertOption:function(t){return function(r){return nt(at)}}},kh={convertOption:function(t){return function(r){return nt(at)}}},gh={convertOption:function(t){return function(r){return S.create}}},Ch={convertOption:function(t){return function(r){return nt(at)}}},Si={convertOption:function(t){return function(r){return nt(at)}}},Xf={convertOption:function(t){return function(r){return nt(at)}}},Kf={convertOption:function(t){return function(r){return nt(at)}}},Yf={convertOption:function(t){return function(r){return nt(at)}}},Zf={convertOption:function(t){return function(r){return nt(at)}}},tl={convertOption:function(t){return function(r){return nt(at)}}},hh={convertOption:function(t){return function(r){return nt(at)}}},Eh={convertOption:function(t){return function(r){return nt(at)}}},Sh={convertOption:function(t){return function(r){return nt(at)}}},rd={convertOption:function(t){return function(r){return nt(at)}}},rf={convertOption:function(t){return function(r){return nt(at)}}},X_={convertOption:function(t){return function(r){return nt(at)}}},K_={convertOption:function(t){return function(r){return nt(at)}}};var rl={convertOption:function(t){return function(r){return nt(at)}}},Th={convertOption:function(t){return function(r){return nt(at)}}},xh={convertOption:function(t){return function(r){return nt(at)}}},Fh={convertOption:function(t){return function(r){return nt(at)}}},ed={convertOption:function(t){return function(r){return nt(at)}}};var Oh={convertOption:function(t){return function(r){return nt(at)}}},nd={convertOption:function(t){return function(r){return nt(at)}}},kn={convertOption:function(t){return function(r){return nt(at)}}},cn={convertOption:function(t){return function(r){return nt(at)}}},ad={convertOption:function(t){return function(r){return nt(at)}}},tm={convertOption:function(t){return function(r){return nt(at)}}},uI=function(t){return t.toPeriodicOscSpec},Ti=function(t){return{convertOption:function(r){return function(e){return uI(t)}}}},ud=function(t){return t.toInitializeWaveShaper},$h=function(t){return t.toInitializeTriangleOsc},wh=function(t){return t.toInitializeStereoPanner},Mh=function(t){return t.toInitializeSquareOsc},Ph=function(t){return t.toInitializeSinOsc},Ih=function(t){return t.toInitializeSawtoothOsc},Rh=function(t){return t.toInitializeRecorder},od=function(t){return t.toInitializePlayBuf},Nh=function(t){return t.toInitializePeriodicOsc},Lh=function(t){return t.toInitializePeaking},Bh=function(t){return t.toInitializeNotch},Wh=function(t){return t.toInitializeMicrophone},Uh=function(t){return t.toInitializeLowshelf},id=function(t){return t.toInitializeLowpass},cd=function(t){return t.toInitializeLoopBuf},qh=function(t){return t.toInitializeIIRFilter},Hh=function(t){return t.toInitializeHighshelf},fd=function(t){return t.toInitializeHighpass},zh=function(t){return t.toInitializeGain},Vh=function(t){return t.toInitializeDynamicsCompressor},ld=function(t){return t.toInitializeDelay},Gh=function(t){return t.toInitializeConvolver},Jh=function(t){return t.toInitializeConstant},_d=function(t){return t.toInitializeBandpass},pd=function(t){return t.toInitializeAllpass};var oI={oversample:_h},iI=function(t){return{toInitializeWaveShaper:function(r){return da(t)(zP.value)(oI)(r)}}},jh={toInitializeWaveShaper:function(){var t=ud(iI(St(Et()(z(ht)(aI)()()()({reflectSymbol:function(){return"curve"}})))(Ct()())));return function(r){return t(function(e){return{curve:e}}(r))}}()},cI=function(){return{bufferOffset:0,playbackRate:1,duration:w.value}}(),Y_=function(t){return{toInitializePlayBuf:function(r){return da(t)(VP.value)(cI)(r)}}},Ga={toInitializePlayBuf:function(){var t=od(Y_(St(Et()(z(ht)(Q_)()()()({reflectSymbol:function(){return"buffer"}})))(Ct()())));return function(r){return t(function(e){return{buffer:e}}(r))}}()},fI={},xi=function(t){return{toInitializePeriodicOsc:function(r){return da(t)(GP.value)(fI)(r)}}},lI={q:1,gain:0},el=function(t){return{toInitializePeaking:function(r){return da(t)(JP.value)(lI)(r)}}};var _I={q:1},nl=function(t){return{toInitializeNotch:function(r){return da(t)(jP.value)(_I)(r)}}};var pI={gain:0},Qh=function(t){return{toInitializeLowshelf:function(r){return da(t)(QP.value)(pI)(r)}}};var sI={q:1},sd=function(t){return{toInitializeLowpass:function(r){return da(t)(XP.value)(sI)(r)}}},rm={toInitializeLowpass:function(){var t=id(sd(St(Et()(z(ht)(rd)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},mI=function(){return{loopStart:0,loopEnd:0,playbackRate:1,duration:w.value}}(),ef=function(t){return{toInitializeLoopBuf:function(r){return da(t)(KP.value)(mI)(r)}}},Cr={toInitializeLoopBuf:function(){var t=cd(ef(St(Et()(z(ht)(rf)()()()({reflectSymbol:function(){return"buffer"}})))(Ct()())));return function(r){return t(function(e){return{buffer:e}}(r))}}()},vI={gain:0},Xh=function(t){return{toInitializeHighshelf:function(r){return da(t)(YP.value)(vI)(r)}}};var DI={q:1},md=function(t){return{toInitializeHighpass:function(r){return da(t)(ZP.value)(DI)(r)}}},au={toInitializeHighpass:function(){var t=fd(md(St(Et()(z(ht)(ed)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},dI=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),Kh=function(t){return{toInitializeDynamicsCompressor:function(r){return da(t)(tI.value)(dI)(r)}}},bI={maxDelayTime:1},vd=function(t){return{toInitializeDelay:function(r){return da(t)(rI.value)(bI)(r)}}},Ze={toInitializeDelay:function(){var t=ld(vd(St(Et()(z(ht)(nd)()()()({reflectSymbol:function(){return"delayTime"}})))(Ct()())));return function(r){return t(function(e){return{delayTime:e}}(r))}}()},yI={q:1},fn=function(t){return{toInitializeBandpass:function(r){return da(t)(eI.value)(yI)(r)}}},Dd={toInitializeBandpass:function(){var t=_d(fn(St(Et()(z(ht)(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},AI={q:1},em=function(t){return{toInitializeAllpass:function(r){return da(t)(nI.value)(AI)(r)}}},dd={toInitializeAllpass:function(){var t=pd(em(St(Et()(z(ht)(tm)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()};var CI=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var hI=function(){function t(){}return t.value=new t,t}();var nm={convertOption:function(t){return function(r){return nt(at)}}},am={convertOption:function(t){return function(r){return nt(at)}}};var EI=function(t){return t.toInitializeAnalyser},we=function(t){if(t instanceof m_)return w.value;if(t instanceof Wo)return new S(t.value0);throw new Error("Failed pattern match at Ocarina.Control (line 37, column 1 - line 37, column 38): "+[t.constructor.name])},uu=Qk({doLogic:Sc,ids:function(){var t=ae();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}})({fromElt:O(),connectToParent:function(t){return function(r){return t.connectXToY({from:r.id,to:r.parent})}}});var SI=function(){return{cb:function(t){return l(vt)(l(vt)(void 0))},fftSize:QD.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:KD.value,channelInterpretation:XD.value}}(),um=function(t){return{toInitializeAnalyser:function(r){return da(t)(hI.value)(SI)(r)}}};var TI=function(t){return function(r){var e=Wh(t)(r),n=function(a){return function(u){return Sr(function(o){return function(i){return function(){var p=u.ids();a.raiseId(p)();var _=o(l(C)(u.makeMicrophone({id:p,parent:a.parent,scope:we(a.scope),microphone:e.microphone})))(i)();return function(){return i(u.deleteFromCache({id:p}))(),_()}}}})}};return new F(n)}},Z_=function(t){return TI(t)};var Hn=eu({doLogic:Sc,ids:function(){var t=ae();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}}),xI=function(t){return function(r){return function(e){return function(n){var a=EI(t)(r),u=function(o){return function(i){return Sr(function(f){return function(p){return function(){var s=i.ids();o.raiseId(s)();var v=f(Yt(_t)(A)([l(C)(i.makeAnalyser({id:s,parent:o.parent,scope:we(o.scope),cb:a.cb,fftSize:cD(2)(function(){if(a.fftSize instanceof Js)return 7;if(a.fftSize instanceof js)return 8;if(a.fftSize instanceof HC)return 9;if(a.fftSize instanceof zC)return 10;if(a.fftSize instanceof QD)return 11;if(a.fftSize instanceof VC)return 12;if(a.fftSize instanceof GC)return 13;throw new Error("Failed pattern match at Ocarina.Control (line 197, column 27 - line 204, column 40): "+[a.fftSize.constructor.name])}()),maxDecibels:a.maxDecibels,minDecibels:a.minDecibels,smoothingTimeConstant:a.smoothingTimeConstant,channelCount:a.channelCount,channelCountMode:function(){if(a.channelCountMode instanceof uh)return"explicit";if(a.channelCountMode instanceof KD)return"max";if(a.channelCountMode instanceof ah)return"clamped-max";throw new Error("Failed pattern match at Ocarina.Control (line 210, column 41 - line 213, column 52): "+[a.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(a.channelInterpretation instanceof XD)return"speakers";if(a.channelInterpretation instanceof nh)return"discrete";throw new Error("Failed pattern match at Ocarina.Control (line 214, column 46 - line 216, column 47): "+[a.channelInterpretation.constructor.name])}()})),m(g)(function(c){return We()()()({cb:function(D){return i.setAnalyserNodeCb({id:s,cb:D})}})(c)})(e),Hn({parent:new S(s),scope:o.scope,raiseId:function(c){return l(_r)(void 0)}})(i)(L(n))]))(p)();return function(){return p(i.deleteFromCache({id:s}))(),v()}}}})}};return new F(u)}}}},om=function(t){return function(r){return xI(t)(r)(T(A))}},Yh=function(t){return function(r){return function(e){var n=Gh(t)(r),a=function(u){return function(o){return Sr(function(i){return function(f){return function(){var _=o.ids();u.raiseId(_)();var s=i(Yt(_t)(A)([l(C)(o.makeConvolver({id:_,parent:u.parent,scope:we(u.scope),buffer:n.buffer})),Hn({parent:new S(_),scope:u.scope,raiseId:function(v){return l(_r)(void 0)}})(o)(L(e))]))(f)();return function(){return f(o.deleteFromCache({id:_}))(),s()}}}})}};return new F(a)}}},FI=function(){return function(){return function(t){return function(r){return function(e){return function(n){return function(a){var u=qh(t)(n)(r)(e),o=function(i){return function(f){return Sr(function(p){return function(_){return function(){var v=f.ids();i.raiseId(v)();var c=p(Yt(_t)(A)([l(C)(f.makeIIRFilter({id:v,parent:i.parent,scope:we(i.scope),feedforward:zc()(u.feedforward),feedback:zc()(u.feedback)})),Hn({parent:new S(v),scope:i.scope,raiseId:function(D){return l(_r)(void 0)}})(f)(L(a))]))(_)();return function(){return _(f.deleteFromCache({id:v}))(),c()}}}})}};return new F(o)}}}}}}},Zh=function(){return function(){return function(t){return FI()()(t)(d.value)(d.value)}}},bd=function(t){return function(r){return function(e){var n=Rh(t)(r),a=function(u){return function(o){return Sr(function(i){return function(f){return function(){var _=o.ids();u.raiseId(_)();var s=i(Yt(_t)(A)([l(C)(o.makeRecorder({id:_,parent:u.parent,scope:we(u.scope),cb:n.cb})),Hn({parent:new S(_),scope:u.scope,raiseId:function(v){return l(_r)(void 0)}})(o)(e)]))(f)();return function(){return f(o.deleteFromCache({id:_}))(),s()}}}})}};return new F(a)}}},OI=function(t){return function(r){return Sr(function(e){return function(n){return function(){var u=r.ids();return n(r.makeSpeaker({id:u}))(),e(Hn({parent:new S(u),scope:new Wo("toplevel"),raiseId:function(o){return l(_r)(void 0)}})(r)(L(t)))(n)()}}})}},nf=OI,Lt=function(t){return function(r){return function(e){return Ue(t)(r)(T(A))(e)}}},Ue=function(t){return function(r){return function(e){return function(n){var a=zh(t)(r),u=function(o){return function(i){return Sr(function(f){return function(p){return function(){var s=i.ids();o.raiseId(s)();var v=f(Yt(_t)(A)([l(C)(i.makeGain({id:s,parent:o.parent,scope:we(o.scope),gain:a.gain})),Ce(Tr)(m(g)(function(c){return We()()()({gain:t0(638)(o.scope)(i)(function(D){return i.setGain(function(M){return{id:s,gain:M}}(D))})})(c)})(e)),Hn({parent:new S(s),scope:o.scope,raiseId:function(c){return l(_r)(void 0)}})(i)(L(n))]))(p)();return function(){return p(i.deleteFromCache({id:s}))(),v()}}}})}};return new F(u)}}}},t0=CI("tmpResolveAU","Ocarina.Control",function(){var t=function(){var o=me()({reflectSymbol:function(){return"unit"}})(d.value);return function(i){return jf(o(i))}}(),r=function(){var o=me()({reflectSymbol:function(){return"sudden"}})(d.value);return function(i){return jf(o(i))}}(),e=function(){var o=me()({reflectSymbol:function(){return"numeric"}})(d.value);return function(i){return jf(o(i))}}(),n=function(){var o=me()({reflectSymbol:function(){return"envelope"}})(d.value);return function(i){return jf(o(i))}}(),a=function(){var o=me()({reflectSymbol:function(){return"cancel"}})(d.value);return function(i){return jf(o(i))}}(),u=function(o){return function(i){return function(f){return function(p){return We()()()({numeric:function(){var _=l(C);return function(s){return _(f(e(s)))}}(),envelope:function(){var _=l(C);return function(s){return _(f(n(s)))}}(),cancel:function(){var _=l(C);return function(s){return _(f(a(s)))}}(),sudden:function(){var _=l(C);return function(s){return _(f(r(s)))}}(),unit:function(_){var s=Lt(ct)(1)([_.u]);return Sr(function(v){return function(c){return function(){var M=je(w.value)();return v(X(K)(Hn({parent:w.value,scope:o,raiseId:function(et){return ir(xe)(du(new S(et))(M))}})(i)(s))(Sr(function(et){return function(Ft){return function(){return function(){var ve=Tn(M)();if(ve instanceof w)return void 0;if(ve instanceof S)return Ft(f(t({i:ve.value0})))();throw new Error("Failed pattern match at Ocarina.Control (line 1829, column 38 - line 1831, column 67): "+[ve.constructor.name])}(),l(_r)(void 0)}}})))(c)()}}})}})(p)}}}};return u}),fe=t0(1807),$I=function(t){return function(r){return function(e){var n=cd(t)(r),a=function(u){return function(o){return Sr(function(i){return function(f){return function(){var _=o.ids();u.raiseId(_)();var s=i(Yt(_t)(A)([l(C)(o.makeLoopBuf({id:_,parent:u.parent,scope:we(u.scope),buffer:n.buffer,playbackRate:n.playbackRate,loopStart:n.loopStart,loopEnd:n.loopEnd,duration:n.duration})),Ce(Tr)(m(g)(function(v){return We()()()({buffer:function(c){return l(C)(o.setBuffer({id:_,buffer:c}))},playbackRate:fe(u.scope)(o)(function(c){return o.setPlaybackRate(function(D){return{id:_,playbackRate:D}}(c))}),loopStart:function(c){return l(C)(o.setLoopStart({id:_,loopStart:c}))},loopEnd:function(c){return l(C)(o.setLoopEnd({id:_,loopEnd:c}))},onOff:function(c){return l(C)(o.setOnOff({id:_,onOff:c}))}})(v)})(e))]))(f)();return function(){return f(o.deleteFromCache({id:_}))(),s()}}}})}};return new F(a)}}},fr=function(t){return $I(t)};var wI=function(t){return function(r){return function(e){var n=Nh(t)(r),a=function(u){return function(o){return Sr(function(i){return function(f){return function(){var _=o.ids();u.raiseId(_)();var s=i(Yt(_t)(A)([l(C)(o.makePeriodicOsc({id:_,parent:u.parent,scope:we(u.scope),frequency:n.frequency,spec:n.spec})),Ce(Tr)(m(g)(function(v){return We()()()({frequency:fe(u.scope)(o)(function(c){return o.setFrequency(function(D){return{id:_,frequency:D}}(c))}),onOff:function(c){return l(C)(o.setOnOff({id:_,onOff:c}))},spec:function(c){return l(C)(o.setPeriodicOsc({id:_,spec:c}))}})(v)})(e))]))(f)();return function(){return f(o.deleteFromCache({id:_}))(),s()}}}})}};return new F(a)}}},Fi=function(t){return wI(t)};var MI=function(t){return function(r){return function(e){var n=od(t)(r),a=function(u){return function(o){return Sr(function(i){return function(f){return function(){var _=o.ids();u.raiseId(_)();var s=i(Yt(_t)(A)([l(C)(o.makePlayBuf({id:_,parent:u.parent,scope:we(u.scope),buffer:n.buffer,playbackRate:n.playbackRate,bufferOffset:n.bufferOffset,duration:n.duration})),Ce(Tr)(m(g)(function(v){return We()()()({buffer:function(c){return l(C)(o.setBuffer({id:_,buffer:c}))},playbackRate:fe(u.scope)(o)(function(c){return o.setPlaybackRate(function(D){return{id:_,playbackRate:D}}(c))}),bufferOffset:function(c){return l(C)(o.setBufferOffset({id:_,bufferOffset:c}))},onOff:function(c){return l(C)(o.setOnOff({id:_,onOff:c}))},duration:function(c){return l(C)(o.setDuration({id:_,duration:c}))}})(v)})(e))]))(f)();return function(){return f(o.deleteFromCache({id:_}))(),s()}}}})}};return new F(a)}}},zn=function(t){return MI(t)};var PI=function(t){return function(r){return function(e){var n=Ih(t)(r),a=function(u){return function(o){return Sr(function(i){return function(f){return function(){var _=o.ids();u.raiseId(_)();var s=i(Yt(_t)(A)([l(C)(o.makeSawtoothOsc({id:_,parent:u.parent,scope:we(u.scope),frequency:n.frequency})),Ce(Tr)(m(g)(function(v){return We()()()({frequency:fe(u.scope)(o)(function(c){return o.setFrequency(function(D){return{id:_,frequency:D}}(c))}),onOff:function(c){return l(C)(o.setOnOff({id:_,onOff:c}))}})(v)})(e))]))(f)();return function(){return f(o.deleteFromCache({id:_}))(),s()}}}})}};return new F(a)}}},r0=function(t){return PI(t)};var II=function(t){return function(r){return function(e){var n=Ph(t)(r),a=function(u){return function(o){return Sr(function(i){return function(f){return function(){var _=o.ids();u.raiseId(_)();var s=i(Yt(_t)(A)([l(C)(o.makeSinOsc({id:_,parent:u.parent,scope:we(u.scope),frequency:n.frequency})),Ce(Tr)(m(g)(function(v){return We()()()({frequency:fe(u.scope)(o)(function(c){return o.setFrequency(function(D){return{id:_,frequency:D}}(c))}),onOff:function(c){return l(C)(o.setOnOff({id:_,onOff:c}))}})(v)})(e))]))(f)();return function(){return f(o.deleteFromCache({id:_}))(),s()}}}})}};return new F(a)}}},af=function(t){return II(t)},e0=function(t){return function(r){return af(t)(r)(T(A))}},RI=function(t){return function(r){return function(e){var n=Mh(t)(r),a=function(u){return function(o){return Sr(function(i){return function(f){return function(){var _=o.ids();u.raiseId(_)();var s=i(Yt(_t)(A)([l(C)(o.makeSquareOsc({id:_,parent:u.parent,scope:we(u.scope),frequency:n.frequency})),Ce(Tr)(m(g)(function(v){return We()()()({frequency:fe(u.scope)(o)(function(c){return o.setFrequency(function(D){return{id:_,frequency:D}}(c))}),onOff:function(c){return l(C)(o.setOnOff({id:_,onOff:c}))}})(v)})(e))]))(f)();return function(){return f(o.deleteFromCache({id:_}))(),s()}}}})}};return new F(a)}}},tp=function(t){return RI(t)},n0=function(t){return function(r){return tp(t)(r)(T(A))}},NI=function(t){return function(r){return function(e){var n=$h(t)(r),a=function(u){return function(o){return Sr(function(i){return function(f){return function(){var _=o.ids();u.raiseId(_)();var s=i(Yt(_t)(A)([l(C)(o.makeTriangleOsc({id:_,parent:u.parent,scope:we(u.scope),frequency:n.frequency})),Ce(Tr)(m(g)(function(v){return We()()()({frequency:fe(u.scope)(o)(function(c){return o.setFrequency(function(D){return{id:_,frequency:D}}(c))}),onOff:function(c){return l(C)(o.setOnOff({id:_,onOff:c}))}})(v)})(e))]))(f)();return function(){return f(o.deleteFromCache({id:_}))(),s()}}}})}};return new F(a)}}},im=function(t){return NI(t)};var LI=function(t){return function(r){return function(e){return function(n){var a=pd(t)(r),u=function(o){return function(i){return Sr(function(f){return function(p){return function(){var s=i.ids();o.raiseId(s)();var v=f(Yt(_t)(A)([l(C)(i.makeAllpass({id:s,parent:o.parent,scope:we(o.scope),frequency:a.frequency,q:a.q})),Ce(Tr)(m(g)(function(c){return We()()()({frequency:fe(o.scope)(i)(function(D){return i.setFrequency(function(M){return{id:s,frequency:M}}(D))}),q:fe(o.scope)(i)(function(D){return i.setQ(function(M){return{id:s,q:M}}(D))})})(c)})(e)),Hn({parent:new S(s),scope:o.scope,raiseId:function(c){return l(_r)(void 0)}})(i)(L(n))]))(p)();return function(){return p(i.deleteFromCache({id:s}))(),v()}}}})}};return new F(u)}}}},rp=function(t){return function(r){return function(e){return LI(t)(r)(T(A))(e)}}},yd=function(t){return function(r){return function(e){return function(n){var a=_d(t)(r),u=function(o){return function(i){return Sr(function(f){return function(p){return function(){var s=i.ids();o.raiseId(s)();var v=f(Yt(_t)(A)([l(C)(i.makeBandpass({id:s,parent:o.parent,scope:we(o.scope),frequency:a.frequency,q:a.q})),Ce(Tr)(m(g)(function(c){return We()()()({frequency:fe(o.scope)(i)(function(D){return i.setFrequency(function(M){return{id:s,frequency:M}}(D))}),q:fe(o.scope)(i)(function(D){return i.setQ(function(M){return{id:s,q:M}}(D))})})(c)})(e)),Hn({parent:new S(s),scope:o.scope,raiseId:function(c){return l(_r)(void 0)}})(i)(L(n))]))(p)();return function(){return p(i.deleteFromCache({id:s}))(),v()}}}})}};return new F(u)}}}},gn=function(t){return function(r){return function(e){return yd(t)(r)(T(A))(e)}}},ep=function(t){return function(r){return function(e){return function(n){var a=ld(t)(r),u=function(o){return function(i){return Sr(function(f){return function(p){return function(){var s=i.ids();o.raiseId(s)();var v=f(Yt(_t)(A)([l(C)(i.makeDelay({id:s,parent:o.parent,scope:we(o.scope),delayTime:a.delayTime,maxDelayTime:a.maxDelayTime})),Ce(Tr)(m(g)(function(c){return We()()()({delayTime:fe(o.scope)(i)(function(D){return i.setDelay(function(M){return{id:s,delayTime:M}}(D))})})(c)})(e)),Hn({parent:new S(s),scope:o.scope,raiseId:function(c){return l(_r)(void 0)}})(i)(L(n))]))(p)();return function(){return p(i.deleteFromCache({id:s}))(),v()}}}})}};return new F(u)}}}},yo=function(t){return function(r){return function(e){return ep(t)(r)(T(A))(e)}}},BI=function(t){return function(r){return function(e){return function(n){var a=Vh(t)(r),u=function(o){return function(i){return Sr(function(f){return function(p){return function(){var s=i.ids();o.raiseId(s)();var v=f(Yt(_t)(A)([l(C)(i.makeDynamicsCompressor({id:s,parent:o.parent,scope:we(o.scope),threshold:a.threshold,ratio:a.ratio,knee:a.knee,attack:a.attack,release:a.release})),Ce(Tr)(m(g)(function(c){return We()()()({threshold:fe(o.scope)(i)(function(D){return i.setThreshold(function(M){return{id:s,threshold:M}}(D))}),ratio:fe(o.scope)(i)(function(D){return i.setRatio(function(M){return{id:s,ratio:M}}(D))}),knee:fe(o.scope)(i)(function(D){return i.setKnee(function(M){return{id:s,knee:M}}(D))}),attack:fe(o.scope)(i)(function(D){return i.setAttack(function(M){return{id:s,attack:M}}(D))}),release:fe(o.scope)(i)(function(D){return i.setRelease(function(M){return{id:s,release:M}}(D))})})(c)})(e)),Hn({parent:new S(s),scope:o.scope,raiseId:function(c){return l(_r)(void 0)}})(i)(L(n))]))(p)();return function(){return p(i.deleteFromCache({id:s}))(),v()}}}})}};return new F(u)}}}},a0=function(t){return function(r){return BI(t)(r)(T(A))}},WI=function(){return function(t){return function(r){return tD()()()({doLogic:Sc,ids:function(){var e=ae();return function(n){return function(a){return a.ids}(e(n))}}(),disconnectElement:function(e){return function(n){return e.disconnectXFromY({from:n.id,to:n.parent})}},toElt:function(e){return e}})({fromEltO1:O(),fromEltO2:O(),toElt:O(),wrapElt:function(e){return Lt(ct)(1)([e])},giveNewParent:function(e){return function(n){return function(a){return e.connectXToY({from:n.id,to:n.parent})}}},deleteFromCache:function(){var e=ae();return function(n){return function(a){return a.deleteFromCache}(e(n))}}()})(t)(Fa(xn)(m(b_)(function(e){return e(void 0)}))(O()(r)))}}},wa=function(t){return function(r){return WI()(qv(t))(Fa(xn)(Hv()()()()()({reflectType:function(){return 0}})(d.value))(r))}};var Ad=function(t){return function(r){return function(e){return function(n){var a=fd(t)(r),u=function(o){return function(i){return Sr(function(f){return function(p){return function(){var s=i.ids();o.raiseId(s)();var v=f(Yt(_t)(A)([l(C)(i.makeHighpass({id:s,parent:o.parent,scope:we(o.scope),frequency:a.frequency,q:a.q})),Ce(Tr)(m(g)(function(c){return We()()()({frequency:fe(o.scope)(i)(function(D){return i.setFrequency(function(M){return{id:s,frequency:M}}(D))}),q:fe(o.scope)(i)(function(D){return i.setQ(function(M){return{id:s,q:M}}(D))})})(c)})(e)),Hn({parent:new S(s),scope:o.scope,raiseId:function(c){return l(_r)(void 0)}})(i)(L(n))]))(p)();return function(){return p(i.deleteFromCache({id:s}))(),v()}}}})}};return new F(u)}}}},al=function(t){return function(r){return function(e){return Ad(t)(r)(T(A))(e)}}},UI=function(t){return function(r){return function(e){return function(n){var a=Hh(t)(r),u=function(o){return function(i){return Sr(function(f){return function(p){return function(){var s=i.ids();o.raiseId(s)();var v=f(Yt(_t)(A)([l(C)(i.makeHighshelf({id:s,parent:o.parent,scope:we(o.scope),frequency:a.frequency,gain:a.gain})),Ce(Tr)(m(g)(function(c){return We()()()({frequency:fe(o.scope)(i)(function(D){return i.setFrequency(function(M){return{id:s,frequency:M}}(D))}),gain:fe(o.scope)(i)(function(D){return i.setGain(function(M){return{id:s,gain:M}}(D))})})(c)})(e)),Hn({parent:new S(s),scope:o.scope,raiseId:function(c){return l(_r)(void 0)}})(i)(L(n))]))(p)();return function(){return p(i.deleteFromCache({id:s}))(),v()}}}})}};return new F(u)}}}},u0=function(t){return function(r){return function(e){return UI(t)(r)(T(A))(e)}}},o0=function(t){return function(r){return function(e){return function(n){var a=id(t)(r),u=function(o){return function(i){return Sr(function(f){return function(p){return function(){var s=i.ids();o.raiseId(s)();var v=f(Yt(_t)(A)([l(C)(i.makeLowpass({id:s,parent:o.parent,scope:we(o.scope),frequency:a.frequency,q:a.q})),Ce(Tr)(m(g)(function(c){return We()()()({frequency:fe(o.scope)(i)(function(D){return i.setFrequency(function(M){return{id:s,frequency:M}}(D))}),q:fe(o.scope)(i)(function(D){return i.setQ(function(M){return{id:s,q:M}}(D))})})(c)})(e)),Hn({parent:new S(s),scope:o.scope,raiseId:function(c){return l(_r)(void 0)}})(i)(L(n))]))(p)();return function(){return p(i.deleteFromCache({id:s}))(),v()}}}})}};return new F(u)}}}},ul=function(t){return function(r){return function(e){return o0(t)(r)(T(A))(e)}}},qI=function(t){return function(r){return function(e){return function(n){var a=Uh(t)(r),u=function(o){return function(i){return Sr(function(f){return function(p){return function(){var s=i.ids();o.raiseId(s)();var v=f(Yt(_t)(A)([l(C)(i.makeLowshelf({id:s,parent:o.parent,scope:we(o.scope),frequency:a.frequency,gain:a.gain})),Ce(Tr)(m(g)(function(c){return We()()()({frequency:fe(o.scope)(i)(function(D){return i.setFrequency(function(M){return{id:s,frequency:M}}(D))}),gain:fe(o.scope)(i)(function(D){return i.setGain(function(M){return{id:s,gain:M}}(D))})})(c)})(e)),Hn({parent:new S(s),scope:o.scope,raiseId:function(c){return l(_r)(void 0)}})(i)(L(n))]))(p)();return function(){return p(i.deleteFromCache({id:s}))(),v()}}}})}};return new F(u)}}}},i0=function(t){return function(r){return function(e){return qI(t)(r)(T(A))(e)}}},HI=function(t){return function(r){return function(e){return function(n){var a=Bh(t)(r),u=function(o){return function(i){return Sr(function(f){return function(p){return function(){var s=i.ids();o.raiseId(s)();var v=f(Yt(_t)(A)([l(C)(i.makeNotch({id:s,parent:o.parent,scope:we(o.scope),frequency:a.frequency,q:a.q})),Ce(Tr)(m(g)(function(c){return We()()()({frequency:fe(o.scope)(i)(function(D){return i.setFrequency(function(M){return{id:s,frequency:M}}(D))}),q:fe(o.scope)(i)(function(D){return i.setQ(function(M){return{id:s,q:M}}(D))})})(c)})(e)),Hn({parent:new S(s),scope:o.scope,raiseId:function(c){return l(_r)(void 0)}})(i)(L(n))]))(p)();return function(){return p(i.deleteFromCache({id:s}))(),v()}}}})}};return new F(u)}}}},ol=function(t){return function(r){return function(e){return HI(t)(r)(T(A))(e)}}},zI=function(t){return function(r){return function(e){return function(n){var a=wh(t)(r),u=function(o){return function(i){return Sr(function(f){return function(p){return function(){var s=i.ids();o.raiseId(s)();var v=f(Yt(_t)(A)([l(C)(i.makeStereoPanner({id:s,parent:o.parent,scope:we(o.scope),pan:a.pan})),Ce(Tr)(m(g)(function(c){return We()()()({pan:fe(o.scope)(i)(function(D){return i.setPan(function(M){return{id:s,pan:M}}(D))})})(c)})(e)),Hn({parent:new S(s),scope:o.scope,raiseId:function(c){return l(_r)(void 0)}})(i)(L(n))]))(p)();return function(){return p(i.deleteFromCache({id:s}))(),v()}}}})}};return new F(u)}}}},c0=function(t){return function(r){return zI(t)(r)(T(A))}},VI=function(t){return function(r){return function(e){return function(n){var a=Lh(t)(r),u=function(o){return function(i){return Sr(function(f){return function(p){return function(){var s=i.ids();o.raiseId(s)();var v=f(Yt(_t)(A)([l(C)(i.makePeaking({id:s,parent:o.parent,scope:we(o.scope),frequency:a.frequency,q:a.q,gain:a.gain})),Ce(Tr)(m(g)(function(c){return We()()()({frequency:fe(o.scope)(i)(function(D){return i.setFrequency(function(M){return{id:s,frequency:M}}(D))}),q:fe(o.scope)(i)(function(D){return i.setQ(function(M){return{id:s,q:M}}(D))}),gain:fe(o.scope)(i)(function(D){return i.setGain(function(M){return{id:s,gain:M}}(D))})})(c)})(e)),Hn({parent:new S(s),scope:o.scope,raiseId:function(c){return l(_r)(void 0)}})(i)(L(n))]))(p)();return function(){return p(i.deleteFromCache({id:s}))(),v()}}}})}};return new F(u)}}}},il=function(t){return function(r){return function(e){return VI(t)(r)(T(A))(e)}}},f0=function(t){return function(r){return function(e){var n=ud(t)(r),a=function(u){return function(o){return Sr(function(i){return function(f){return function(){var _=o.ids();u.raiseId(_)();var s=i(Yt(_t)(A)([l(C)(o.makeWaveShaper({id:_,parent:u.parent,scope:we(u.scope),curve:n.curve,oversample:n.oversample})),Hn({parent:new S(_),scope:u.scope,raiseId:function(v){return l(_r)(void 0)}})(o)(L(e))]))(f)();return function(){return f(o.deleteFromCache({id:_}))(),s()}}}})}};return new F(a)}}},GI=function(t){return function(r){return function(e){var n=Jh(t)(r),a=function(u){return function(o){return Sr(function(i){return function(f){return function(){var _=o.ids();u.raiseId(_)();var s=i(Yt(_t)(A)([l(C)(o.makeConstant({id:_,parent:u.parent,scope:we(u.scope),offset:n.offset})),Ce(Tr)(m(g)(function(v){return We()()()({offset:fe(u.scope)(o)(function(c){return o.setOffset(function(D){return{id:_,offset:D}}(c))}),onOff:function(c){return l(C)(o.setOnOff({id:_,onOff:c}))}})(v)})(e))]))(f)();return function(){return f(o.deleteFromCache({id:_}))(),s()}}}})}};return new F(a)}}},cm=function(t){return GI(t)};function kd(){window.scrollTo(0,0)}var Ao=function(t){return t.sequential},$n=function(t){return t.parallel};var Cn=function(t){return function(r){return new F(Q("button")(t)(O()(L(O()(G(j)(r))))))}};var Ma=function(){var t={},r="Pure",e="Throw",n="Catch",a="Sync",u="Async",o="Bind",i="Bracket",f="Fork",p="Sequential",_="Map",s="Apply",v="Alt",c="Cons",D="Resume",M="Release",et="Finalizer",Ft="Finalized",Wt="Forked",Xr="Fiber",ve="Thunk";function lt(Ht,Yr,ze,_e){this.tag=Ht,this._1=Yr,this._2=ze,this._3=_e}function zt(Ht){var Yr=function(ze,_e,ur){return new lt(Ht,ze,_e,ur)};return Yr.tag=Ht,Yr}function Fr(Ht){return new lt(r,void 0)}function Pt(Ht){try{Ht()}catch(Yr){setTimeout(function(){throw Yr},0)}}function le(Ht,Yr,ze){try{return Yr(ze())}catch(_e){return Ht(_e)}}function Vn(Ht,Yr,ze){try{return Yr(ze)()}catch(_e){return ze(Ht(_e))(),Fr}}var qe=function(){var Ht=1024,Yr=0,ze=0,_e=new Array(Ht),ur=!1;function Ot(){var ee;for(ur=!0;Yr!==0;)Yr--,ee=_e[ze],_e[ze]=void 0,ze=(ze+1)%Ht,ee();ur=!1}return{isDraining:function(){return ur},enqueue:function(ee){var Rr,Ne;Yr===Ht&&(Ne=ur,Ot(),ur=Ne),_e[(ze+Yr)%Ht]=ee,Yr++,ur||Ot()}}}();function Ea(Ht){var Yr={},ze=0,_e=0;return{register:function(ur){var Ot=ze++;ur.onComplete({rethrow:!0,handler:function(ee){return function(){_e--,delete Yr[Ot]}}})(),Yr[Ot]=ur,_e++},isEmpty:function(){return _e===0},killAll:function(ur,Ot){return function(){if(_e===0)return Ot();var ee=0,Rr={};function Ne(Ae){Rr[Ae]=Yr[Ae].kill(ur,function(tn){return function(){delete Rr[Ae],ee--,Ht.isLeft(tn)&&Ht.fromLeft(tn)&&setTimeout(function(){throw Ht.fromLeft(tn)},0),ee===0&&Ot()}})()}for(var Jn in Yr)Yr.hasOwnProperty(Jn)&&(ee++,Ne(Jn));return Yr={},ze=0,_e=0,function(Ae){return new lt(a,function(){for(var tn in Rr)Rr.hasOwnProperty(tn)&&Rr[tn]()})}}}}}var ca=0,He=1,Gu=2,ui=3,_f=4,Na=5,Gn=6;function Cc(Ht,Yr,ze){var _e=0,ur=ca,Ot=ze,ee=null,Rr=null,Ne=null,Jn=null,Ae=null,tn=0,hc=0,Ou=null,Wi=!0;function Ui(or){for(var lr,Vr,Kr;;)switch(lr=null,Vr=null,Kr=null,ur){case Gu:ur=He;try{Ot=Ne(Ot),Jn===null?Ne=null:(Ne=Jn._1,Jn=Jn._2)}catch(fa){ur=Na,ee=Ht.left(fa),Ot=null}break;case ui:Ht.isLeft(Ot)?(ur=Na,ee=Ot,Ot=null):Ne===null?ur=Na:(ur=Gu,Ot=Ht.fromRight(Ot));break;case He:switch(Ot.tag){case o:Ne&&(Jn=new lt(c,Ne,Jn)),Ne=Ot._2,ur=He,Ot=Ot._1;break;case r:Ne===null?(ur=Na,Ot=Ht.right(Ot._1)):(ur=Gu,Ot=Ot._1);break;case a:ur=ui,Ot=le(Ht.left,Ht.right,Ot._1);break;case u:ur=_f,Ot=Vn(Ht.left,Ot._1,function(fa){return function(){_e===or&&(_e++,qe.enqueue(function(){_e===or+1&&(ur=ui,Ot=fa,Ui(_e))}))}});return;case e:ur=Na,ee=Ht.left(Ot._1),Ot=null;break;case n:Ne===null?Ae=new lt(c,Ot,Ae,Rr):Ae=new lt(c,Ot,new lt(c,new lt(D,Ne,Jn),Ae,Rr),Rr),Ne=null,Jn=null,ur=He,Ot=Ot._1;break;case i:tn++,Ne===null?Ae=new lt(c,Ot,Ae,Rr):Ae=new lt(c,Ot,new lt(c,new lt(D,Ne,Jn),Ae,Rr),Rr),Ne=null,Jn=null,ur=He,Ot=Ot._1;break;case f:ur=ui,lr=Cc(Ht,Yr,Ot._2),Yr&&Yr.register(lr),Ot._1&&lr.run(),Ot=Ht.right(lr);break;case p:ur=He,Ot=oi(Ht,Yr,Ot._1);break}break;case Na:if(Ne=null,Jn=null,Ae===null)ur=Gn,Ot=Rr||ee||Ot;else switch(lr=Ae._3,Kr=Ae._1,Ae=Ae._2,Kr.tag){case n:Rr&&Rr!==lr&&tn===0?ur=Na:ee&&(ur=He,Ot=Kr._2(Ht.fromLeft(ee)),ee=null);break;case D:Rr&&Rr!==lr&&tn===0||ee?ur=Na:(Ne=Kr._1,Jn=Kr._2,ur=Gu,Ot=Ht.fromRight(Ot));break;case i:tn--,ee===null&&(Vr=Ht.fromRight(Ot),Ae=new lt(c,new lt(M,Kr._2,Vr),Ae,lr),(Rr===lr||tn>0)&&(ur=He,Ot=Kr._3(Vr)));break;case M:Ae=new lt(c,new lt(Ft,Ot,ee),Ae,Rr),ur=He,Rr&&Rr!==lr&&tn===0?Ot=Kr._1.killed(Ht.fromLeft(Rr))(Kr._2):ee?Ot=Kr._1.failed(Ht.fromLeft(ee))(Kr._2):Ot=Kr._1.completed(Ht.fromRight(Ot))(Kr._2),ee=null,tn++;break;case et:tn++,Ae=new lt(c,new lt(Ft,Ot,ee),Ae,Rr),ur=He,Ot=Kr._1;break;case Ft:tn--,ur=Na,Ot=Kr._1,ee=Kr._2;break}break;case Gn:for(var Ge in Ou)Ou.hasOwnProperty(Ge)&&(Wi=Wi&&Ou[Ge].rethrow,Pt(Ou[Ge].handler(Ot)));Ou=null,Rr&&ee?setTimeout(function(){throw Ht.fromLeft(ee)},0):Ht.isLeft(Ot)&&Wi&&setTimeout(function(){if(Wi)throw Ht.fromLeft(Ot)},0);return;case ca:ur=He;break;case _f:return}}function Ve(or){return function(){if(ur===Gn)return Wi=Wi&&or.rethrow,or.handler(Ot)(),function(){};var lr=hc++;return Ou=Ou||{},Ou[lr]=or,function(){Ou!==null&&delete Ou[lr]}}}function br(or,lr){return function(){if(ur===Gn)return lr(Ht.right(void 0))(),function(){};var Vr=Ve({rethrow:!1,handler:function(){return lr(Ht.right(void 0))}})();switch(ur){case ca:Rr=Ht.left(or),ur=Gn,Ot=Rr,Ui(_e);break;case _f:Rr===null&&(Rr=Ht.left(or)),tn===0&&(ur===_f&&(Ae=new lt(c,new lt(et,Ot(or)),Ae,Rr)),ur=Na,Ot=null,ee=null,Ui(++_e));break;default:Rr===null&&(Rr=Ht.left(or)),tn===0&&(ur=Na,Ot=null,ee=null)}return Vr}}function Lr(or){return function(){var lr=Ve({rethrow:!1,handler:or})();return ur===ca&&Ui(_e),lr}}return{kill:br,join:Lr,onComplete:Ve,isSuspended:function(){return ur===ca},run:function(){ur===ca&&(qe.isDraining()?Ui(_e):qe.enqueue(function(){Ui(_e)}))}}}function gp(Ht,Yr,ze,_e){var ur=0,Ot={},ee=0,Rr={},Ne=new Error("[ParAff] Early exit"),Jn=null,Ae=t;function tn(Ve,br,Lr){var or=br,lr=null,Vr=null,Kr=0,Ge={},fa,Tl;t:for(;;)switch(fa=null,or.tag){case Wt:if(or._3===t&&(fa=Ot[or._1],Ge[Kr++]=fa.kill(Ve,function(rT){return function(){Kr--,Kr===0&&Lr(rT)()}})),lr===null)break t;or=lr._2,Vr===null?lr=null:(lr=Vr._1,Vr=Vr._2);break;case _:or=or._2;break;case s:case v:lr&&(Vr=new lt(c,lr,Vr)),lr=or,or=or._1;break}if(Kr===0)Lr(Ht.right(void 0))();else for(Tl=0,fa=Kr;Tl<fa;Tl++)Ge[Tl]=Ge[Tl]();return Ge}function hc(Ve,br,Lr){var or,lr,Vr,Kr,Ge,fa;Ht.isLeft(Ve)?(or=Ve,lr=null):(lr=Ve,or=null);t:for(;;){if(Vr=null,Kr=null,Ge=null,fa=null,Jn!==null)return;if(br===null){_e(or||lr)();return}if(br._3!==t)return;switch(br.tag){case _:or===null?(br._3=Ht.right(br._1(Ht.fromRight(lr))),lr=br._3):br._3=or;break;case s:if(Vr=br._1._3,Kr=br._2._3,or){if(br._3=or,Ge=!0,fa=ee++,Rr[fa]=tn(Ne,or===Vr?br._2:br._1,function(){return function(){delete Rr[fa],Ge?Ge=!1:Lr===null?hc(or,null,null):hc(or,Lr._1,Lr._2)}}),Ge){Ge=!1;return}}else{if(Vr===t||Kr===t)return;lr=Ht.right(Ht.fromRight(Vr)(Ht.fromRight(Kr))),br._3=lr}break;case v:if(Vr=br._1._3,Kr=br._2._3,Vr===t&&Ht.isLeft(Kr)||Kr===t&&Ht.isLeft(Vr))return;if(Vr!==t&&Ht.isLeft(Vr)&&Kr!==t&&Ht.isLeft(Kr))or=lr===Vr?Kr:Vr,lr=null,br._3=or;else if(br._3=lr,Ge=!0,fa=ee++,Rr[fa]=tn(Ne,lr===Vr?br._2:br._1,function(){return function(){delete Rr[fa],Ge?Ge=!1:Lr===null?hc(lr,null,null):hc(lr,Lr._1,Lr._2)}}),Ge){Ge=!1;return}break}Lr===null?br=null:(br=Lr._1,Lr=Lr._2)}}function Ou(Ve){return function(br){return function(){delete Ot[Ve._1],Ve._3=br,hc(br,Ve._2._1,Ve._2._2)}}}function Wi(){var Ve=He,br=ze,Lr=null,or=null,lr,Vr;t:for(;;)switch(lr=null,Vr=null,Ve){case He:switch(br.tag){case _:Lr&&(or=new lt(c,Lr,or)),Lr=new lt(_,br._1,t,t),br=br._2;break;case s:Lr&&(or=new lt(c,Lr,or)),Lr=new lt(s,t,br._2,t),br=br._1;break;case v:Lr&&(or=new lt(c,Lr,or)),Lr=new lt(v,t,br._2,t),br=br._1;break;default:Vr=ur++,Ve=Na,lr=br,br=new lt(Wt,Vr,new lt(c,Lr,or),t),lr=Cc(Ht,Yr,lr),lr.onComplete({rethrow:!1,handler:Ou(br)})(),Ot[Vr]=lr,Yr&&Yr.register(lr)}break;case Na:if(Lr===null)break t;Lr._1===t?(Lr._1=br,Ve=He,br=Lr._2,Lr._2=t):(Lr._2=br,br=Lr,or===null?Lr=null:(Lr=or._1,or=or._2))}for(Ae=br,Vr=0;Vr<ur;Vr++)Ot[Vr].run()}function Ui(Ve,br){Jn=Ht.left(Ve);var Lr;for(var or in Rr)if(Rr.hasOwnProperty(or)){Lr=Rr[or];for(or in Lr)Lr.hasOwnProperty(or)&&Lr[or]()}Rr=null;var lr=tn(Ve,Ae,br);return function(Vr){return new lt(u,function(Kr){return function(){for(var Ge in lr)lr.hasOwnProperty(Ge)&&lr[Ge]();return Fr}})}}return Wi(),function(Ve){return new lt(u,function(br){return function(){return Ui(Ve,br)}})}}function oi(Ht,Yr,ze){return new lt(u,function(_e){return function(){return gp(Ht,Yr,ze,_e)}})}return lt.EMPTY=t,lt.Pure=zt(r),lt.Throw=zt(e),lt.Catch=zt(n),lt.Sync=zt(a),lt.Async=zt(u),lt.Bind=zt(o),lt.Bracket=zt(i),lt.Fork=zt(f),lt.Seq=zt(p),lt.ParMap=zt(_),lt.ParApply=zt(s),lt.ParAlt=zt(v),lt.Fiber=Cc,lt.Supervisor=Ea,lt.Scheduler=qe,lt.nonCanceler=Fr,lt}(),l0=Ma.Pure,tR=Ma.Throw;function _0(t){return function(r){return r.tag===Ma.Pure.tag?Ma.Pure(t(r._1)):Ma.Bind(r,function(e){return Ma.Pure(t(e))})}}function p0(t){return function(r){return Ma.Bind(t,r)}}var s0=Ma.Sync;function m0(t){return function(r){return Ma.ParMap(t,r)}}function v0(t){return function(r){return Ma.ParApply(t,r)}}function D0(t){return function(r){return Ma.ParAlt(t,r)}}var cl=Ma.Async;function d0(t,r){return function(){return Ma.Fiber(t,null,r)}}var rR=function(){function t(e,n){return e===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,e)}function r(e,n){return e===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(e,n){return Ma.Async(function(a){return function(){var u=t(n,a(e()));return function(){return Ma.Sync(function(){return e(r(n,u))})}}})}}(),b0=Ma.Seq;var nR=function(t){return function(r){return function(e){var n=Ao(t),a=Ie(t.Applicative1())(r)(function(){var u=$n(t);return function(o){return u(e(o))}}());return function(u){return n(a(u))}}}},y0=function(t){return function(r){return function(e){var n=Ao(t),a=Rn(r)(t.Applicative1())(function(){var u=$n(t);return function(o){return u(e(o))}}());return function(u){return n(a(u))}}}},A0=function(t){return function(r){return nR(t)(r)(nt(at))}};var aR=function(t){return t};var g0=function(t){return t};var ap=function(t){return t.toDuration};var C0={fromDuration:ev()()(aR)(function(t){return t*1e3}),toDuration:ev()()(g0)(function(t){return t/1e3})};var h0=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var oR=function(t){return t};var ll={map:m0},Oi={map:_0};var iR=function(){var t=function(n){if(n instanceof rr)return n.value0;if(n instanceof tr)return Au("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): "+[n.constructor.name])},r=function(n){if(n instanceof tr)return n.value0;if(n instanceof rr)return Au("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): "+[n.constructor.name])},e=function(n){if(n instanceof tr)return!0;if(n instanceof rr)return!1;throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): "+[n.constructor.name])};return{isLeft:e,fromLeft:r,fromRight:t,left:tr.create,right:rr.create}}(),cR=function(t){return d0(iR,t)},ko=function(t){return function(){var e=cR(t)();return e.run(),e}},ti=function(){var t=ir(N);return function(r){return t(ko(r))}}();var $i={apply:v0,Functor0:function(){return ll}};var gd={Applicative0:function(){return Ca},Bind1:function(){return Pe}},Pe={bind:p0,Apply0:function(){return Cd(0)}},Ca={pure:l0,Apply0:function(){return Cd(0)}},Cd=h0("applyAff","Effect.Aff",function(){return{apply:su(gd),Functor0:function(){return Oi}}}),E0=Cd(71);var Me={liftEffect:s0,Monad0:function(){return gd}},S0=function(){var t=ye(Me);return function(r){return oR(E(t(r)))}}(),T0=function(t){return cl(function(r){return m(N)(S0)(t.join(r))})};var x0=function(t){return function(r){return mt(Pe)(ye(Me)(r.isSuspended))(function(e){return e?ye(Me)(ir(N)(r.kill(t,E(l(vt)(void 0))))):cl(function(n){return m(N)(S0)(r.kill(t,n))})})}};var wn={parallel:pt,sequential:b0,Monad0:function(){return gd},Applicative1:function(){return fR(0)}},fR=h0("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=$n(wn),r=l(Ca);return function(e){return t(r(e))}}(),Apply0:function(){return $i}}});var lR={append:function(t){return function(r){return function(e){return A0(wn)(_t)([t(e),r(e)])}}}};var _R=E(l(Ca)(void 0)),F0={mempty:_R,Semigroup0:function(){return lR}};var O0={alt:D0,Functor0:function(){return ll}};var $0=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),up=function(){function t(){}return t.value=new t,t}(),uf=function(){function t(){}return t.value=new t,t}(),op=function(){function t(){}return t.value=new t,t}(),of=function(){function t(){}return t.value=new t,t}(),ip=function(){function t(){}return t.value=new t,t}(),cp=function(){function t(){}return t.value=new t,t}(),w0=function(){function t(){}return t.value=new t,t}(),fm=function(){function t(){}return t.value=new t,t}(),lm=function(){function t(){}return t.value=new t,t}(),fp=function(){function t(){}return t.value=new t,t}(),lp=function(){function t(){}return t.value=new t,t}(),M0=function(){function t(){}return t.value=new t,t}(),_l=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),hd=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var pR="numeric",sR="sudden",mR="unit",vR="cancel",DR="step",dR="linear",bR="exponential",yR="envelope",P0=function(t,r,e,n){if(e.type===sR)t.value=e.value.n;else if(e.type===mR)r.id&&kR(r.id,n),n.units[e.value.i].main.connect(t),r.id=e.value.i;else if(e.type===pR)t[e.value.t.type===DR?"setValueAtTime":e.value.t.type===dR?"linearRampToValueAtTime":e.value.t.type===bR?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](e.value.n,e.value.o);else if(e.type===vR)e.value.hold?t.cancelAndHoldAtTime(e.value.o):t.cancelScheduledValues(e.value.o);else if(e.type===yR){let a=e.value.o;t.cancelScheduledValues(Math.max(0,a)),t.setValueCurveAtTime(e.value.p,a,e.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(e))},AR=function(t,r,e,n,a){return n[e]||(n[e]={}),P0(r.parameters.get(e),n[e],a,t)},Hu=function(t,r,e,n,a){return n[e]||(n[e]={}),P0(r[e],n[e],a,t)},he=function(t,r,e,n){let a=t("@fan@")(u=>u)(e);n.scopes[a]||(n.scopes[a]=[]),n.scopes[a].push(r),n.units[r].scope=a},Ee=function(t,r){r.toConnect[t]&&(r.toConnect[t].forEach(function(e){e.w?r.units[e.w]?e.f():(r.toConnect[e.w]||(r.toConnect[e.w]=[]),r.toConnect[e.w].push({f:e.f})):e.f()}),delete r.toConnect[t])},Se=function(t,r,e,n){t()(a=>I0(r,a,n))(e)},I0=function(t,r,e){var n=function(){e.units[t].audioOutgoing.push(r),e.units[t].pendingOn||(e.units[t].main.connect(e.units[r].main),e.units[r].se&&e.units[t].main.connect(e.units[r].se))};if(!e.units[t]){e.toConnect[t]||(e.toConnect[t]=[]);var a={f:n};r!==t&&!e.units[r]&&(a.w=r),e.toConnect[t].push(a);return}if(!e.units[r]){e.toConnect[r]||(e.toConnect[r]=[]);var a={f:n};r!==t&&!e.units[t]&&(a.w=t),e.toConnect[r].push(a);return}n()};function Ed(t){return function(r){return function(){delete r.units[t.id]}}}function Sd(t){return function(r){return function(){I0(t.from,t.to,r)}}}var kR=function(t,r){if(r.units[t].scope==="@fan@")return;let e=r.units[t].scope;r.scopes[e].forEach(n=>{delete r.units[n]}),delete r.scopes[e]};function Td(t){return function(r){return function(){var e=t.from,n=t.to;if(!r.units[e]||(r.units[e].audioOutgoing=r.units[e].audioOutgoing.filter(function(u){return u!==n}),r.units[e].main.disconnect(r.units[n].main),r.units[n].se&&r.units[e].main.disconnect(r.units[n].se),r.units[e].scope==="@fan@"))return;let a=r.units[e].scope;r.scopes[a].forEach(u=>{delete r.units[u]}),delete r.scopes[a]}}}var xd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"allpass",Q:r.q,frequency:r.frequency})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Fd=t=>r=>e=>()=>{var n=r.id,a=r.cb,u=new AnalyserNode(e.context,r),o=a(u)();e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:a,analyser:o,main:e.context.createGain(),se:u},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Od=t=>r=>e=>()=>{var n=r.id,a=r.options;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(e.context,a.name,{numberOfInputs:a.numberOfInputs,numberOfOutputs:a.numberOfOutputs,outputChannelCount:a.outputChannelCount,parameterData:a.parameterData,processorOptions:a.processorOptions})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},$d=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"bandpass",Q:r.q,frequency:r.frequency})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},wd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new ConstantSourceNode(o,i)},u={offset:r.offset};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Md=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(e.context,{buffer:r.buffer})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Pd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(e.context,{delayTime:r.delayTime,maxDelayTime:r.maxDelayTime})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Id=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(e.context,{knee:r.knee,ratio:r.ratio,threshold:r.threshold,attack:r.attack,release:r.release})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Rd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(e.context,{gain:r.gain})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Nd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highpass",Q:r.q,frequency:r.frequency})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Ld=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highshelf",frequency:r.frequency,gain:r.gain})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Bd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(e.context,{feedforward:r.feedforward,feedback:r.feedback})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Wd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new AudioBufferSourceNode(o,i)},u={loop:!0,buffer:r.buffer,loopStart:r.loopStart,loopEnd:r.loopEnd,playbackRate:r.playbackRate};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Ud=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowpass",Q:r.q,frequency:r.frequency})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},qd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowshelf",frequency:r.frequency,gain:r.gain})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Hd=t=>r=>e=>()=>{var n=r.id,a=r.element,u=function(){var o=e.context.createMediaElementSource(a);return o};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:u,resumeClosure:{},main:u()},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},zd=t=>r=>e=>()=>{var n=r.id;e.units[r.id]={main:e.context.createMediaStreamSource(r.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Vd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"notch",frequency:r.frequency,Q:r.q})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Gd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"peaking",frequency:r.frequency,Q:r.q,gain:r.gain})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Jd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){var f={frequency:i.frequency,periodicWave:i.spec.type==="wave"?i.spec.value:$b(e.context)(i.spec.value.real)(i.spec.value.img)()},p=new OscillatorNode(o,f);return p},u={frequency:r.frequency,type:"custom",spec:r.spec};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},jd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){var f={loop:i.loop,buffer:i.buffer,playbackRate:i.playbackRate};return new AudioBufferSourceNode(o,f)},u={loop:!1,buffer:r.buffer,playbackRate:r.playbackRate,bufferOffset:r.bufferOffset,duration:t(void 0)(o=>o)(r.duration)};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Qd=t=>r=>e=>()=>{var n=r.id,a=r.cb,u=e.context.createMediaStreamDestination(),o=new MediaRecorder(u.stream);a(o)(),o.start(),e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:a,recorder:o,main:e.context.createGain(),se:u},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Xd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:r.frequency,type:"sawtooth"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Kd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:r.frequency,type:"sine"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},Yd=t=>r=>()=>{r.units[t.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:r.context.createGain(),se:r.context.destination}},Zd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(e.context,{pan:r.pan})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},tb=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:r.frequency,type:"square"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},rb=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:r.frequency,type:"triangle"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)},eb=t=>r=>e=>()=>{var n=r.id,a=r.curve,u=r.oversample;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(e.context,{curve:a,oversample:u.type})},he(t,n,r.scope,e),Ee(n,e),Se(t,n,r.parent,e)};function nb(t){return function(r){return function(){var e=t.id,n=t.cb;r.units[e].analyserOrig!==n&&(r.units[e].analyser&&r.units[e].analyser(),r.units[e].analyser=n(r.units[e].se)(),r.units[e].analyserOrig=n)}}}function ab(t){return function(r){return function(){var e=t.cb,n=t.id;if(r.units[n].recorderOrig!==e){r.units[n].recorder&&r.units[n].recorder.stop();var a=e;r.units[n].recorderOrig=e;var u=new MediaRecorder(r.units[n].se);a(u)(),u.start()}}}}function ub(t){return function(r){return function(){var e=t.id,n=t.curve;r.units[e].main.curve=n}}}function ob(t){return function(r){return function(){var e=t.id,n=t.paramName,a=t.paramValue;AR(r,r.units[e].main,n,r.units[e].controllers,a)}}}var zu=function(t,r,e){r.resume&&t.value.n!==void 0&&(r.resume[e]=t.value.n)};function ib(t){return function(r){return function(){var e=t.id,n=t.gain;Hu(r,r.units[e].main,"gain",r.units[e].controllers,n),zu(n,r.units[e],"gain")}}}function cb(t){return function(r){return function(){var e=t.id,n=t.q;Hu(r,r.units[e].main,"Q",r.units[e].controllers,n),zu(n,r.units[e],"Q")}}}function fb(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].resume&&(r.units[e].resume.buffer=n)}}}function lb(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].main.buffer=n}}}function _b(t){return function(r){return function(){var e=t.id,n=t.spec;r.units[e].resume&&(r.units[e].resume.spec=n)}}}function pb(t){return function(r){return function(){var e=t.id,n=t.pan;Hu(r,r.units[e].main,"pan",r.units[e].controllers,n),zu(n,r.units[e],"pan")}}}function sb(t){return function(r){return function(){var e=t.id,n=t.threshold;Hu(r,r.units[e].main,"threshold",r.units[e].controllers,n),zu(n,r.units[e],"threshold")}}}function mb(t){return function(r){return function(){var e=t.id,n=t.loopStart;r.units[e].main.loopStart=n,r.units[e].resume.loopStart=n}}}function vb(t){return function(r){return function(){var e=t.id,n=t.loopEnd;r.units[e].main.loopEnd=n,r.units[e].resume.loopEnd=n}}}function Db(t){return function(r){return function(){var e=t.id,n=t.bufferOffset;r.units[e].resume.bufferOffset=n}}}function db(t){return function(r){return function(e){return function(){var n=r.id,a=r.duration;e.units[n].duration=t(void 0)(u=>u)(a)}}}}function bb(t){return function(r){return function(){var e=t.id,n=t.release;Hu(r,r.units[e].main,"release",r.units[e].controllers,n),zu(n,r.units[e],"release")}}}function yb(t){return function(r){return function(){var e=t.id,n=t.offset;Hu(r,r.units[e].main,"offset",r.units[e].controllers,n),zu(n,r.units[e],"offset")}}}function Ab(t){return function(r){return function(){var e=t.id,n=t.ratio;Hu(r,r.units[e].main,"ratio",r.units[e].controllers,n),zu(n,r.units[e],"ratio")}}}function kb(t){return function(r){return function(){var e=t.id,n=t.attack;Hu(r,r.units[e].main,"attack",r.units[e].controllers,n),zu(n,r.units[e],"attack")}}}function gb(t){return function(r){return function(){var e=t.id,n=t.knee;Hu(r,r.units[e].main,"knee",r.units[e].controllers,n),zu(n,r.units[e],"knee")}}}function Cb(t){return function(r){return function(){var e=t.id,n=t.delayTime;Hu(r,r.units[e].main,"delayTime",r.units[e].controllers,n),zu(n,r.units[e],"delayTime")}}}function hb(t){return function(r){return function(){var e=t.id,n=t.playbackRate;Hu(r,r.units[e].main,"playbackRate",r.units[e].controllers,n),zu(n,r.units[e],"playbackRate")}}}function Eb(t){return function(r){return function(){var e=t.id,n=t.frequency;Hu(r,r.units[e].main,"frequency",r.units[e].controllers,n),zu(n,r.units[e],"frequency")}}}function Sb(t){return function(r){return function(){var e=t.id,n=t.onOff;n.x.type==="on"?gR(e)(n)(r)():n.x.type==="off"&&CR(e)(n)(r)()}}}var gR=function(t){return function(r){return function(e){return function(){if(!e.units[t].onOff){e.units[t].pendingOn=!1,e.units[t].onOff=!0,e.units[t].main=e.units[t].createClosure(e.context,e.units[t].resume);for(var n=0;n<e.units[t].audioOutgoing.length;n++){var a=e.units[t].audioOutgoing[n];e.units[t].main.connect(e.units[a].main),e.units[a].se&&e.units[t].main.connect(e.units[a].se)}e.units[t].resume&&e.units[t].resume.bufferOffset?typeof e.units[t].resume.duration=="number"?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset,e.units[t].resume.duration):e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset):e.units[t].resume&&e.units[t].resume.loopStart?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.loopStart):e.units[t].main.start(e.deprecatedWriteHead+r.o)}}}}},CR=function(t){return function(r){return function(e){return function(){if(!!e.units[t].onOff){e.units[t].onOff=!1;var n=e.units[t].main;n.addEventListener("ended",()=>{n.disconnect()}),n.stop(e.deprecatedWriteHead+r.o)}}}}};function Tb(t){for(var r=new Float32Array(t.length),e=0;e<t.length;e++)r[e]=t[e];return r}function _m(t){return function(){t.stop()}}function xb(t){return function(r){return function(e){return function(){var n=[];e.ondataavailable=function(a){n.push(a.data)},e.onstop=function(){var a=new Blob(n,{type:t});r(a)(),n=null}}}}}function Fb(t){return function(r){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:r})}}}function _p(t){return function(){var r=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(r),r}}function Ob(t){return function(){var r=t.createConstantSource();return r.offset.value=0,r.connect(t.destination),r.start(),function(){r.stop(),r.disconnect(t.destination)}}}var $b=function(t){return function(r){return function(e){return function(){for(var n=new Float32Array(r.length),a=new Float32Array(e.length),u=0;u<r.length;u++)n[u]=r[u];for(var u=0;u<e.length;u++)a[u]=e[u];return t.createPeriodicWave(n,a,{disableNormalization:!0})}}}};function mc(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},scopes:{},unsu:{},toConnect:{}}}}function wb(t){return function(){t.close()}}function Mb(t){return function(){return fetch(t).then(function(r){return r.arrayBuffer()},function(r){return console.error("Error fetching buffer",r),Promise.reject(r)})}}function Pb(t){return function(r){return function(){return t.decodeAudioData(r)}}}function Ib(){return new(window.AudioContext||window.webkitAudioContext)}function Rb(t){return function(){return t.state}}function pp(t){return function(){return t.currentTime}}function R0(t){return function(r){return function(e){return function(){t.then(e,r)}}}}var SR=function(t){return function(r){return cl(function(e){return Cp(N)(vr(F0))(R0(r)(function(n){return e(tr.create(t(n)))()})(function(n){return e(rr.create(n))()}))})}};var TR=function(t){return Wa(function(r){return jo("Promise failed, couldn't extract JS Error or String")})(nt(at))(UD(X(WD(kv)(Pu))(GD(Pu)("Error")(t))(m(H_(Xu))(jo)(JD(Pu)(t)))))},N0=SR(TR),pm=function(t){return mt(Pe)(ye(Me)(t))(N0)};function Nb(t){return function(){return URL.createObjectURL(t)}}var L0=function(t){return function(r){return function(e){return yr(xb(t))(e)(function(){var n=jn(Zn)(r);return function(a){return n(Nb(a))}}())}}};var cf=function(t){return{ids:function(){var e=Tn(t)(),n=Gt(Ba)(pc(jc(Es))({newSeed:cc(e),size:5}));return ir(xe)(eo(Mr(Ka)(1))(t))(),n},deleteFromCache:Ed,disconnectXFromY:Td,connectXToY:Sd,makeAllpass:xd(Ut),makeAnalyser:Fd(Ut),makeAudioWorkletNode:Od(Ut),makeBandpass:$d(Ut),makeConstant:wd(Ut),makeConvolver:Md(Ut),makeDelay:Pd(Ut),makeDynamicsCompressor:Id(Ut),makeGain:Rd(Ut),makeHighpass:Nd(Ut),makeHighshelf:Ld(Ut),makeIIRFilter:Bd(Ut),makeLoopBuf:Wd(Ut),makeLowpass:Ud(Ut),makeLowshelf:qd(Ut),makeMediaElement:Hd(Ut),makeMicrophone:zd(Ut),makeNotch:Vd(Ut),makePeaking:Gd(Ut),makePeriodicOsc:Jd(Ut),makePlayBuf:jd(Ut),makeRecorder:Qd(Ut),makeSawtoothOsc:Xd(Ut),makeSinOsc:Kd(Ut),makeSpeaker:Yd,makeSquareOsc:tb(Ut),makeStereoPanner:Zd(Ut),makeTriangleOsc:rb(Ut),makeWaveShaper:eb(Ut),setAnalyserNodeCb:nb,setMediaRecorderCb:ab,setWaveShaperCurve:ub,setAudioWorkletParameter:ob,setBuffer:fb,setConvolverBuffer:lb,setDuration:db(Ut),setPeriodicOsc:_b,setOnOff:Sb,setBufferOffset:Db,setLoopStart:mb,setLoopEnd:vb,setRatio:Ab,setOffset:yb,setAttack:kb,setGain:ib,setQ:cb,setPan:pb,setThreshold:sb,setRelease:bb,setKnee:gb,setDelay:Cb,setPlaybackRate:hb,setFrequency:Eb}},xt=function(t){return function(r){return mt(Pe)(pm(Mb(r)))(function(){var e=Pb(t);return function(n){return pm(e(n))}}())}},sp=function(t){var r=ye(t);return function(e){return r(Rb(e))}};var oa=function(t){return ye(t)(Ib)},Vu=function(t){var r=ye(t);return function(e){return r(Ob(e))}},hn=function(t){return function(r){return ye(t)(function(){var n=sp(ce)(r)();return Pn(vt)(n!=="closed")(wb(r))()})}},MR=pt,PR=pt,sm=function(t){return function(r){return m(Oi)(function(e){return{microphone:function(){return t?l(To)(MR(e)):w.value}(),camera:function(){return r?l(To)(PR(e)):w.value}()}})(pm(Fb(t)(r)))}};var ri=function(){function t(){}return t.value=new t,t}(),ei=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Fu=function(){function t(){}return t.value=new t,t}(),ln=kd,wi=function(t){return Ao(wn)(X(O0)($n(wn)(mt(Pe)(T0(t))(ye(Me))))($n(wn)(x0(jo("We navigated away from the page"))(t))))},pl=function(t){return function(r){return function(e){return function(n){return X(t)(l(r)(Fu.value))(n)}}}},Pa=function(t){return function(r){return function(e){return function(n){return X(t)(l(r)(tt(be)(se.value)(te(E(n)))))(m(t.Functor0())(function(a){return tt(be)(se.value)(te(E(dt(kt)(a)(n))))})(m(t.Functor0())(function(a){return a.value0})(e)))}}}},mm=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return m(g)(function(o){return tt(be)(se.value)(te(E(function(){if(o.value0 instanceof ri)return l(vt)(void 0);if(o.value0 instanceof ei)return dt(kt)(dt(kt)(o.value0.value0)(t(l(vt)(void 0))))(r(Fu.value));if(o.value0 instanceof Fu)return function(){o.value1(),r(ri.value)();var f=ko(mt(Pe)(oa(Me))(function(p){return mt(Pe)(Vu(Me)(p))(function(_){return mt(Pe)(e(p))(function(s){return ye(Me)(function(){var c=n(p)(s)(),D=dt(kt)(dt(kt)(c)(_))(hn(ce)(p));return r(new ei(D))(),D})})})}))();return t(function(){return r(Fu.value)(),ti(wi(f))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 57, column 21 - line 75, column 26): "+[o.value0.constructor.name])}())))})(yt(mn)(m(g)(rt.create)(u))(X(K)(l(C)(l(vt)(void 0)))(m(g)(function(o){return o.value0})(a))))}}}}}},Ia=function(t){return function(r){return function(e){return function(){return t(e)(),r(new $0(e))()}}}},vm=function(t){return function(r){return function(e){return function(n){return function(a){return Ke(function(u){return function(o){var i=pl(K)(C)(r)(o);return Rf(X(K)(l(C)(tt(cs)(Jt.value)("cursor: pointer;")))(mm(e)(u)(n)(a)(r)(i)))([un(m(g)(function(f){if(f instanceof Fu)return t;if(f instanceof ri)return"\u23F3";if(f instanceof ei)return"\u{1F6D1}";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 126, column 17 - line 129, column 35): "+[f.constructor.name])})(i))])}})}}}}},Mt=function(t){return function(r){return function(e){return function(n){return Ke(function(a){return function(u){var o=pl(K)(C)(t)(u);return Cn(mm(r)(a)(e)(n)(t)(o))([un(m(g)(function(i){if(i instanceof Fu)return"Turn on";if(i instanceof ri)return"Loading...";if(i instanceof ei)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 100, column 17 - line 103, column 42): "+[i.constructor.name])})(o))])}})}}}};var sl=function(t){return function(r){return function(){var n=mc(t)(),a=ea(ra)(je(0))(),u=Be(nf([new qo(m(g)(function(o){return v_.create(Ek(o))})(r))])(cf(a)))(function(o){return o(n)})();return u}}};var At=function(t){return function(r){return function(){var n=mc(t)(),a=ea(ra)(je(0))(),u=Be(nf(r)(cf(a)))(function(o){return o(n)})();return u}}},Dm=function(t){return function(){var e=oa(ce)();return m(N)(function(n){return dt(kt)(n)(hn(ce)(e))})(At(e)(t))()}};var IR=function(){return d.value}(),B0=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(I()(Z)({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}}))(d.value)(IR)({allpass:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([wa(fr(Cr)(a)(st()))(function(u){return function(o){return Lt(ct)(.2)([u,rp(dd)(700)([rp(em(St(Et()(z(z(ht)(ad)()()()({reflectSymbol:function(){return"q"}}))(tm)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:990,q:20})([u]),rp(dd)(1110)([u,rp(em(St(Et()(z(z(ht)(ad)()()()({reflectSymbol:function(){return"q"}}))(tm)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:2010,q:30})([u])])])])}})])}}))})}}};function Mi(t){return function(e,n,a){if(n===null)return new t(e);var u=e.byteLength,o=t.BYTES_PER_ELEMENT,i=Math.min(u,n>>>0);if(a===null)return new t(e,i);var f=Math.min((u-i)/o,a);return new t(e,i,f)}}var NR=Mi(Uint8ClampedArray),LR=Mi(Uint32Array),BR=Mi(Uint16Array),W0=Mi(Uint8Array),WR=Mi(Int32Array),UR=Mi(Int16Array),qR=Mi(Int8Array),HR=Mi(Float32Array),zR=Mi(Float64Array);function U0(t){for(var r=t.length,e=new Array(r),n=0;n<r;n++)e[n]=t[n];return e}var dm={create:W0,BinaryValue0:function(){}};var bm=function(t){return function(r){return function(){return U0(r)}}};var ml=Cu,vl=Cu,Dl=Cu,ou=Cu,iu=Cu,cu=Cu,fu=Cu,lu=Cu;function ym(t){return t|0}var XR=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},vc=xa(function(t){return function(){var e=Ci(),n=Br(!0)(),a=XR("fx","FRP.Event.Animate",function(){return ir(N)(yr(Fs)(e)(function(){var i=Jr(n)();return Pn(vt)(i)(function(){return t(void 0)(),a(19)()})()}))}),u=a(15);return u(),ne(!1)(n)}});var KR="background-color: rgb(150,30,10);",YR="background-color: rgb(130,60,10);",ZR="background-color: rgb(80,90,10);",tN="background-color: rgb(10,130,10);",rN="background-color: rgb(10,100,0);",eN=Ws(nu)(function(t){return Pr(wr(Ls)()(Va)()(U_))(KR)(Pr(wr(ua)()(dn)()(Va))(YR)(Pr(wr(hu)()(bn)()(dn))(ZR)(Pr(wr(Eu)()(yn)()(bn))(tN)(Pr(wr(Su)()(Tu)()(yn))(rN)(qu)))))}),nN=function(t){return function(r){return function(e){return function(n){return om(um(St(Et()(z(z(ht)(am)()()()({reflectSymbol:function(){return"fftSize"}}))(t)()()()({reflectSymbol:function(){return"cb"}})))(Ct()())))({cb:n,fftSize:js.value})([fr(r)(e)(st())])}}}},aN=function(){return d.value}(),Nr="background-color: rgb(255,255,255,0.0);",Wr=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return function(i){return function(f){return function(p){return function(_){return m(t)(function(s){var v=q_(r)()(q_(n)()(s)(f))(p);return v?tt(u)(Jt.value)(q_(r)()(q_(n)()(eN)(f))(p)):tt(u)(Jt.value)(Nr)})(_)}}}}}}}}}}},uN=function(){return 15/40}(),oN=function(){return 10/40}(),iN=function(){return 7/40}(),cN=function(){return 3/40}(),fN=function(){return 1/40}(),V0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Ocarina provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Ocarina as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
`}})()()(I()(Z)({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))(aN)({analyser:P(Ke(function(n){return function(a){var u=a_(ru)(nt(at))(a),o=pl(K)(C)(e)(function(f){return f.right}(u)),i=function(f){return f.left}(u);return zr([Cn(X(K)(l(C)(tt(Pf)(Jt.value)("cursor: pointer;")))(mm(t)(function(f){return n(rr.create(f))})(function(f){return xt(f)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(f){return function(p){return function(){var s=Br(w.value)(),v=mc(f)(),c=ea(ra)(je(0))(),D=nf([nN(nm)(Cr)(p)(function(et){return function(){return ne(new S(et))(s)(),ne(w.value)(s)}})])(cf(c)),M=Be(X(K)(m(g)(rr.create)(D))(m(g)(tr.create)(vc)))(function(et){if(et instanceof rr)return et.value0(v);if(et instanceof tr)return function(){var Wt=Jr(s)();return pn(vt)(Zr)(Wt)(function(Xr){return function(){var lt=_p(Xr)(),zt=bm(dm)(lt)(),Fr=Br(0)(),Pt=Br(0)(),le=Br(0)(),Vn=Br(0)(),qe=Br(0)(),Ea=Br(0)(),ca=Br(0)(),He=Br(0)(),Gu=Br(0)(),ui=Br(0)(),_f=function(Gn){if(Gn<32)return Fr;if(Gn<64)return Pt;if(Gn<96)return le;if(Gn<128)return Vn;if(Gn<168)return qe;if(Gn<160)return Ea;if(Gn<224)return ca;if(re)return He;throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 146, column 45 - line 154, column 63): "+[Gn.constructor.name])};Zu(zt)(function(Gn){var Cc=ym(Gn);return function(){var oi=Jr(ui)();return _i(Mr(Ka)(Cc))(Gu)(),_i(Mr(Ka)(Cc))(_f(oi))(),_i(Mr(Ka)(1))(ui)()}})();var Na=Rn(SC)(vt)(function(Gn){return function(){var gp=m(N)(jr)(Jr(Gn))(),oi=m(N)(Yu(Nl)(gp))(m(N)(jr)(Jr(Gu)))();return Pr(wr(Ls)()(Va)()(U_))(oi>uN)(Pr(wr(ua)()(dn)()(Va))(oi>oN)(Pr(wr(hu)()(bn)()(dn))(oi>iN)(Pr(wr(Eu)()(yn)()(bn))(oi>cN)(Pr(wr(Su)()(Tu)()(yn))(oi>fN)(qu)))))}})(Pr(wr(gC)()(PD)()(EC))(Fr)(Pr(wr(CC)()(ID)()(PD))(Pt)(Pr(wr(hC)()(U_)()(ID))(le)(Pr(wr(Ls)()(Va)()(U_))(Vn)(Pr(wr(ua)()(dn)()(Va))(qe)(Pr(wr(hu)()(bn)()(dn))(Ea)(Pr(wr(Eu)()(yn)()(bn))(ca)(Pr(wr(Su)()(Tu)()(yn))(He)(qu)))))))))();return n(new tr(Na))()}})()};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 127, column 57 - line 165, column 57): "+[et.constructor.name])})();return function(){return M(),function(){var Wt=sp(ce)(f)();return Pn(vt)(Wt!=="closed")(hn(ce)(f))()}(),n(new tr(Ws(nu)(E(Ws($a)(E(!1))))))()}}}})(e)(o)))([un(m(g)(function(f){if(f instanceof Fu)return"Turn on";if(f instanceof ri)return"Loading...";if(f instanceof ei)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 178, column 31 - line 181, column 56): "+[f.constructor.name])})(o))]),xr(l(C)(tt(bt)(Jt.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")))([xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Oe)(ga)(Oe)(bo)(bt)(ga)(bo)(lu)(lu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Oe)(ga)(Un)(Do)(bt)(ga)(Do)(fu)(lu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Oe)(ga)(Wn)(vo)(bt)(ga)(vo)(cu)(lu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Oe)(ga)(Bn)(mo)(bt)(ga)(mo)(iu)(lu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Oe)(ga)(Ln)(so)(bt)(ga)(so)(ou)(lu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Oe)(ga)($a)(po)(bt)(ga)(po)(Dl)(lu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Oe)(ga)(Ko)(_o)(bt)(ga)(_o)(vl)(lu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Oe)(ga)(Xo)(lo)(bt)(ga)(lo)(ml)(lu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Un)(ka)(Oe)(bo)(bt)(ka)(bo)(lu)(fu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Un)(ka)(Un)(Do)(bt)(ka)(Do)(fu)(fu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Un)(ka)(Wn)(vo)(bt)(ka)(vo)(cu)(fu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Un)(ka)(Bn)(mo)(bt)(ka)(mo)(iu)(fu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Un)(ka)(Ln)(so)(bt)(ka)(so)(ou)(fu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Un)(ka)($a)(po)(bt)(ka)(po)(Dl)(fu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Un)(ka)(Ko)(_o)(bt)(ka)(_o)(vl)(fu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Un)(ka)(Xo)(lo)(bt)(ka)(lo)(ml)(fu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Wn)(Aa)(Oe)(bo)(bt)(Aa)(bo)(lu)(cu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Wn)(Aa)(Un)(Do)(bt)(Aa)(Do)(fu)(cu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Wn)(Aa)(Wn)(vo)(bt)(Aa)(vo)(cu)(cu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Wn)(Aa)(Bn)(mo)(bt)(Aa)(mo)(iu)(cu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Wn)(Aa)(Ln)(so)(bt)(Aa)(so)(ou)(cu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Wn)(Aa)($a)(po)(bt)(Aa)(po)(Dl)(cu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Wn)(Aa)(Ko)(_o)(bt)(Aa)(_o)(vl)(cu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Wn)(Aa)(Xo)(lo)(bt)(Aa)(lo)(ml)(cu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Bn)(ya)(Oe)(bo)(bt)(ya)(bo)(lu)(iu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Bn)(ya)(Un)(Do)(bt)(ya)(Do)(fu)(iu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Bn)(ya)(Wn)(vo)(bt)(ya)(vo)(cu)(iu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Bn)(ya)(Bn)(mo)(bt)(ya)(mo)(iu)(iu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Bn)(ya)(Ln)(so)(bt)(ya)(so)(ou)(iu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Bn)(ya)($a)(po)(bt)(ya)(po)(Dl)(iu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Bn)(ya)(Ko)(_o)(bt)(ya)(_o)(vl)(iu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Bn)(ya)(Xo)(lo)(bt)(ya)(lo)(ml)(iu)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Ln)(ba)(Oe)(bo)(bt)(ba)(bo)(lu)(ou)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Ln)(ba)(Un)(Do)(bt)(ba)(Do)(fu)(ou)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Ln)(ba)(Wn)(vo)(bt)(ba)(vo)(cu)(ou)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Ln)(ba)(Bn)(mo)(bt)(ba)(mo)(iu)(ou)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Ln)(ba)(Ln)(so)(bt)(ba)(so)(ou)(ou)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Ln)(ba)($a)(po)(bt)(ba)(po)(Dl)(ou)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Ln)(ba)(Ko)(_o)(bt)(ba)(_o)(vl)(ou)(i)))([]),xr(X(K)(l(C)(tt(bt)(Jt.value)(Nr)))(Wr(g)(Ln)(ba)(Xo)(lo)(bt)(ba)(lo)(ml)(ou)(i)))([])])])}}))})}}};var _N=function(){return d.value}(),G0=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(I()(Z)({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))(d.value)(_N)({bandpass:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([wa(fr(Cr)(a)(st()))(function(u){return function(o){return Lt(ct)(.8)([gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:400,q:1})([u]),gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:880,q:5})([u]),gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:1200,q:10})([u]),gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:2e3,q:20})([u]),gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:3e3,q:30})([u])])}})])}}))})}}};var sN=function(){return d.value}(),J0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(I()(Z)({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))(sN)({compression:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([a0(Kh(St(Et()(ht))(Ct()())))({})([fr(Cr)(a)(st())])])}}))})}}};var ia=function(){return function(t){var r=Xe(),e=me()({reflectSymbol:function(){return"playbackRate"}})(d.value),n=tf(t);return function(a){return r(e(n(a)))}}},ff=function(){return function(t){var r=Xe(),e=me()({reflectSymbol:function(){return"onOff"}})(d.value),n=oh(t);return function(a){return r(e(n(a)))}}},j0=function(){return function(t){var r=Xe(),e=me()({reflectSymbol:function(){return"offset"}})(d.value),n=tf(t);return function(a){return r(e(n(a)))}}},Q0=function(){var t=Xe(),r=me()({reflectSymbol:function(){return"loopStart"}})(d.value);return function(e){return t(r(e))}},X0=function(){var t=Xe(),r=me()({reflectSymbol:function(){return"loopEnd"}})(d.value);return function(e){return t(r(e))}},En=function(){return function(t){var r=Xe(),e=me()({reflectSymbol:function(){return"gain"}})(d.value),n=tf(t);return function(a){return r(e(n(a)))}}},go=function(){return function(t){var r=Xe(),e=me()({reflectSymbol:function(){return"frequency"}})(d.value),n=tf(t);return function(a){return r(e(n(a)))}}};var dl=function(){return function(t){var r=Xe(),e=me()({reflectSymbol:function(){return"delayTime"}})(d.value),n=tf(t);return function(a){return r(e(n(a)))}}};var vN=function(){return d.value}(),K0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Ocarina uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>pure</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(I()(I()(I()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))(vN)({tf:P(ie("<|>")),txt:P(ie(`run2_
  [ gain_ 0.5
      [ constant 0.0
          ( bangOn <|>
              ( pure $ offset $ AudioEnvelope
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
  ]`)),constant:P(Mt(e)(t)(function(n){return l(Ca)(void 0)})(function(n){return function(a){return At(n)([Lt(ct)(.5)([cm(Zs)(0)(X(K)(st())(l(C)(j0()(On)({d:5,o:.1,p:$o(mi)(function(u){return E(function(){var o=Za(Ku)(u)(3)===0;return o?1:0}())})(en(0)(1920))}))))])])}}))})}}};var dN=function(){return d.value}(),Y0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(I()(Z)({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))(dN)({convolution:P(Mt(e)(t)(function(n){return yt(E0)(m(Oi)(function(a){return function(u){return{loop:a,verb:u}}})(xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(xt(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(a){return At(n)([Yh(Ah)(a.verb)([fr(Cr)(a.loop)(st())])])}}))})}}};var yN=function(){return d.value}(),Z0=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(I()(Z)({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))(d.value)(yN)({delay:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return At(n)([wa(zn(Ga)(a)(st()))(function(u){return function(o){return Lt(ct)(.2)([yo(Ze)(.03)([u]),yo(Ze)(.1)([u]),yo(Ze)(.3)([u]),yo(Ze)(.7)([u])])}})])}}))})}}};var kN=function(){return d.value}(),tE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(I()(Z)({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))(kN)({gain:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return At(n)([Lt(ct)(.1)([fr(Cr)(a)(st())])])}}))})}}};var CN=function(){return d.value}(),rE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(I()(Z)({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))(CN)({highpass:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([al(au)(2e3)([fr(Cr)(a)(st())])])}}))})}}};var EN=function(){return d.value}(),eE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(I()(Z)({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))(EN)({highshelf:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([u0(Xh(St(Et()(z(z(ht)(Th)()()()({reflectSymbol:function(){return"gain"}}))(xh)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:2e3,gain:.4})([fr(Cr)(a)(st())])])}}))})}}};var TN=function(){return d.value}(),nE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="iir">IIR filter</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/IIRFilterNode">IIR filter</a>, or infinite impulse response filter, is the Swiss Army Knife of filters. You can carve out and boost parts of the spectrum with amazing precision. But it comes with a catch: you can't automate the parameters. The parameters are also tough to work with if you're new to IIR filters. In short, you're setting up coefficients for a filter of type:</p>

  <pre><code>x0s0 + x1s1 + x2s2 + ... + y0S0 + y1S1 + y2S2 + ...</code></pre>

  <p>Where <code>s1</code> is the unfiltered signal at time <code>t-1</code>, <code>S0</code> is the <i>filtered</i> signal at time <code>t-1</code>, etc. The xs and ys are often called <i>feedforward</i> and <i>feedback</i> coefficients respectively.</p>

  <p>Because the Web Audio API accepts between 3 and 20 parameters for feedforward and feedback coefficients, Ocarina enforces that through a <a href="https://github.com/bodil/purescript-sized-vectors">sized vector</a>.</p>

  <pre><code>\\{loop, verb} -> run2_
  [ iirFilter
      ( (0.00020298 : 0.0004059599 : 0.00020298 : empty)
          /\\ (1.0126964558 : -1.9991880801 : 0.9873035442 : empty)
      )
      [ loopBuf buf bangOn ]
  ]</code></pre>
  ~iirFilterEx~
  </section>
`}})()()(I()(Z)({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}}))(TN)({iirFilterEx:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([Zh()()(yh(hi)(hi))(new rt(Vc()()(20298e-8)(Vc()()(.0004059599)(Vc()()(20298e-8)(zv))),Vc()()(1.0126964558)(Vc()()(-1.9991880801)(Vc()()(.9873035442)(zv)))))([fr(Cr)(a)(st())])])}}))})}}};var FN=function(){return d.value}(),aE=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(I()(Z)({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))(d.value)(FN)({loopBuf:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(a){return At(n)([fr(ef(St(Et()(z(z(z(z(ht)(rl)()()()({reflectSymbol:function(){return"playbackRate"}}))(K_)()()()({reflectSymbol:function(){return"loopStart"}}))(X_)()()()({reflectSymbol:function(){return"loopEnd"}}))(rf)()()()({reflectSymbol:function(){return"buffer"}})))(Ct()())))({buffer:a,playbackRate:.5,loopStart:.1,loopEnd:.6})(st()),fr(ef(St(Et()(z(z(z(z(ht)(rl)()()()({reflectSymbol:function(){return"playbackRate"}}))(K_)()()()({reflectSymbol:function(){return"loopStart"}}))(X_)()()()({reflectSymbol:function(){return"loopEnd"}}))(rf)()()()({reflectSymbol:function(){return"buffer"}})))(Ct()())))({buffer:a,playbackRate:1,loopStart:.5,loopEnd:1.2})(st()),fr(ef(St(Et()(z(z(ht)(rl)()()()({reflectSymbol:function(){return"playbackRate"}}))(rf)()()()({reflectSymbol:function(){return"buffer"}})))(Ct()())))({buffer:a,playbackRate:1.7})(st())])}}))})}}};var $N=function(){return d.value}(),uE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(I()(Z)({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))($N)({lowpass:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([ul(rm)(215)([fr(Cr)(a)(st())])])}}))})}}};var MN=function(){return d.value}(),oE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(I()(Z)({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))(MN)({lowshelf:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([i0(Qh(St(Et()(z(z(ht)(hh)()()()({reflectSymbol:function(){return"gain"}}))(Eh)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:91,gain:.4})([fr(Cr)(a)(st())])])}}))})}}};var IN=function(){return d.value}(),iE=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(I()(Z)({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))(d.value)(IN)({microphone:P(Mt(e)(t)(function(n){return sm(!0)(!1)})(function(n){return function(a){return At(n)([function(){if(a.microphone instanceof S)return uu(function(u){return Lt(ct)(1)([Z_(j_)(a.microphone.value0),yo(Ze)(.1)([Lt(ct)(.2)([u])])])});if(a.microphone instanceof w)return Lt(ct)(.02)([e0(sc)(440)]);throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Microphone (line 44, column 15 - line 49, column 56): "+[a.microphone.constructor.name])}()])}}))})}}};var NN=function(){return d.value}(),cE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(I()(Z)({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))(NN)({notch:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([ol(nl(St(Et()(z(z(ht)(Zf)()()()({reflectSymbol:function(){return"q"}}))(tl)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:400,q:1})(l(De)(ol(nl(St(Et()(z(z(ht)(Zf)()()()({reflectSymbol:function(){return"q"}}))(tl)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:880,q:5})(l(De)(ol(nl(St(Et()(z(z(ht)(Zf)()()()({reflectSymbol:function(){return"q"}}))(tl)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:1200,q:10})(l(De)(ol(nl(St(Et()(z(z(ht)(Zf)()()()({reflectSymbol:function(){return"q"}}))(tl)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:2e3,q:20})(l(De)(ol(nl(St(Et()(z(z(ht)(Zf)()()()({reflectSymbol:function(){return"q"}}))(tl)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:3e3,q:30})(l(De)(fr(Cr)(a)(st())))))))))))])}}))})}}};var BN=function(){return d.value}(),fE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(I()(Z)({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))(BN)({peaking:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([il(el(St(Et()(z(z(z(ht)(Xf)()()()({reflectSymbol:function(){return"q"}}))(Kf)()()()({reflectSymbol:function(){return"gain"}}))(Yf)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:400,q:1,gain:-20})(l(De)(il(el(St(Et()(z(z(z(ht)(Xf)()()()({reflectSymbol:function(){return"q"}}))(Kf)()()()({reflectSymbol:function(){return"gain"}}))(Yf)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:880,q:5,gain:20})(l(De)(il(el(St(Et()(z(z(z(ht)(Xf)()()()({reflectSymbol:function(){return"q"}}))(Kf)()()()({reflectSymbol:function(){return"gain"}}))(Yf)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:1200,q:10,gain:-20})(l(De)(il(el(St(Et()(z(z(z(ht)(Xf)()()()({reflectSymbol:function(){return"q"}}))(Kf)()()()({reflectSymbol:function(){return"gain"}}))(Yf)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:2e3,q:20,gain:20})(l(De)(il(el(St(Et()(z(z(z(ht)(Xf)()()()({reflectSymbol:function(){return"q"}}))(Kf)()()()({reflectSymbol:function(){return"gain"}}))(Yf)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:3e3,q:30,gain:-20})(l(De)(fr(Cr)(a)(st())))))))))))])}}))})}}};var UN=function(){return d.value}(),lE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(I()(Z)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(UN)({periodic:P(Mt(e)(t)(function(n){return l(Ca)(void 0)})(function(n){return function(a){return At(n)([Lt(ct)(.2)([Fi(xi(St(Et()(z(z(ht)(Ti(Ei(ua)))()()()({reflectSymbol:function(){return"spec"}}))(Si)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:140,spec:new rt(Pr(wr(ua)()(dn)()(Va))(.1)(Pr(wr(hu)()(bn)()(dn))(.2)(Pr(wr(Eu)()(yn)()(bn))(.3)(Pr(wr(Su)()(Tu)()(yn))(.4)(qu)))),Pr(wr(ua)()(dn)()(Va))(.4)(Pr(wr(hu)()(bn)()(dn))(.3)(Pr(wr(Eu)()(yn)()(bn))(.2)(Pr(wr(Su)()(Tu)()(yn))(.1)(qu)))))})(st())])])}}))})}}};var HN=function(){return d.value}(),_E=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(I()(Z)({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))(HN)({playBuf:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(a){return At(n)([zn(Y_(St(Et()(z(z(z(ht)(gh)()()()({reflectSymbol:function(){return"duration"}}))(kh)()()()({reflectSymbol:function(){return"bufferOffset"}}))(Q_)()()()({reflectSymbol:function(){return"buffer"}})))(Ct()())))({buffer:a,duration:3,bufferOffset:4.2})(st())])}}))})}}};var Lb=function(){function t(){}return t.value=new t,t}();var pE={attr:function(t){return function(r){return b({key:"controls",value:B(r)})}}};var Bb=function(){function t(){}return t.value=new t,t}();var sE={attr:function(t){return function(r){return b({key:"src",value:B(r)})}}};var Wb=function(t){return function(r){return new F(Q("audio")(t)(O()(L(O()(G(j)(r))))))}};var lf=function(){function t(){this.head=null,this.last=null,this.size=0}function r(_,s){this.queue=_,this.value=s,this.next=null,this.prev=null}function e(_){this.draining=!1,this.error=null,this.value=_,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function a(_){try{_()}catch(s){setTimeout(function(){throw s},0)}}function u(_,s){var v=new r(_,s);switch(_.size){case 0:_.head=v;break;case 1:v.prev=_.head,_.head.next=v,_.last=v;break;default:v.prev=_.last,_.last.next=v,_.last=v}return _.size++,v}function o(_){var s;switch(_.size){case 0:return null;case 1:s=_.head,_.head=null;break;case 2:s=_.last,_.head.next=null,_.last=null;break;default:s=_.last,_.last=s.prev,_.last.next=null}return s.prev=null,s.queue=null,_.size--,s.value}function i(_){var s;switch(_.size){case 0:return null;case 1:s=_.head,_.head=null;break;case 2:s=_.head,_.last.prev=null,_.head=_.last,_.last=null;break;default:s=_.head,_.head=s.next,_.head.prev=null}return s.next=null,s.queue=null,_.size--,s.value}function f(_){if(_.queue!==null){if(_.queue.last===_){o(_.queue);return}if(_.queue.head===_){i(_.queue);return}_.prev&&(_.prev.next=_.next),_.next&&(_.next.prev=_.prev),_.queue.size--,_.queue=null,_.value=null,_.next=null,_.prev=null}}function p(_,s){if(!s.draining){var v=s.puts,c=s.takes,D=s.reads,M,et,Ft,Wt,Xr;for(s.draining=!0;;){if(M=null,et=null,Ft=null,Wt=s.value,Xr=D.size,s.error!==null){for(Wt=_.left(s.error);M=i(v);)a(M.cb(Wt));for(;et=i(D);)a(et(Wt));for(;Ft=i(c);)a(Ft(Wt));break}if(Wt===n&&(M=i(v))&&(s.value=Wt=M.value),Wt!==n){for(Ft=i(c);Xr--&&(et=i(D));)a(et(_.right(Wt)));Ft!==null&&(s.value=n,a(Ft(_.right(Wt))))}if(M!==null&&a(M.cb(_.right(void 0))),s.value===n&&v.size===0||s.value!==n&&c.size===0)break}s.draining=!1}}return e.EMPTY=n,e.putLast=u,e.takeLast=o,e.takeHead=i,e.deleteCell=f,e.drainVar=p,e}();function Ub(){return new lf(lf.EMPTY)}function mE(t,r,e){return function(){return e.value===lf.EMPTY&&e.error===null?(e.value=r,lf.drainVar(t,e),!0):!1}}function vE(t,r){return function(){var e=r.value;return e===lf.EMPTY?t.nothing:(r.value=lf.EMPTY,lf.drainVar(t,r),t.just(e))}}var QN=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),XN=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),KN=function(){function t(){}return t.value=new t,t}();var DE=function(){return{left:tr.create,right:rr.create,nothing:w.value,just:S.create,killed:QN.create,filled:XN.create,empty:KN.value}}();var dE=function(t){return function(r){return mE(DE,t,r)}};var bE=function(t){return vE(DE,t)};var ZN=function(t){return function(r){return function(e){return function(n){return bd(t)(n)(Z_(r)(e))}}}},tL=function(){return d.value}(),yE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(I()(Z)({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))(tL)({recorder:P(Ke(function(n){return function(a){var u=a_(ru)(nt(at))(a),o=a_(ru)(nt(at))(function(_){return _.left}(u)),i=function(_){return _.right}(o),f=pl(K)(C)(e)(function(_){return _.right}(u)),p=function(_){return _.left}(o);return zr([Cn(X(K)(l(C)(tt(Pf)(Jt.value)("cursor: pointer;")))(m(g)(function(_){return tt(be)(se.value)(te(E(function(){if(_.e instanceof ri)return l(vt)(void 0);if(_.e instanceof ei)return dt(kt)(dt(kt)(dt(kt)(_.e.value0)(t(l(vt)(void 0))))(pn(vt)(Zr)(_.rec)(function(){var s=P_(yD);return function(v){return s(_m(v))}}())))(n(rr.create(Fu.value)));if(_.e instanceof Fu)return function(){_.cncl();var v=Ub();n(new rr(ri.value))();var c=ko(mt(Pe)(m(Oi)(function(D){return D.microphone})(sm(!0)(!1)))(function(D){return ye(Me)(function(){var et=Ut(l(vt)(l(vt)(void 0)))(function(Ft){return function(){var Xr=oa(ce)(),ve=mc(Xr)(),lt=ea(ra)(je(0))(),zt=nf([ZN(td)(j_)(Ft)(function(Pt){return function(){return n(new tr(new rr(Pt)))(),ir(N)(dE(Pt)(v))(),L0("audio/ogg; codecs=opus")(function(Vn){return n(tr.create(tr.create(Vn)))})(Pt)()}})])(cf(lt)),Fr=Be(zt)(function(Pt){return Pt(ve)})();return function(){Fr(),mt(Zn)(bE(v))(Ie(vt)(Zr)(function(){var Vn=P_(yD);return function(qe){return Vn(_m(qe))}}()))();var le=sp(ce)(Xr)();return Pn(vt)(le!=="closed")(hn(ce)(Xr))()}}})(D)();return n(new rr(new ei(et)))(),et})}))();return t(function(){return n(rr.create(Fu.value))(),ti(wi(c))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 67, column 45 - line 111, column 50): "+[_.e.constructor.name])}())))})(yt(mn)(m(g)(sf)(yt(mn)(m(g)(function(_){return function(s){return function(v){return{e:_,cncl:s,rec:v}}}})(f))(X(K)(l(C)(l(vt)(void 0)))(m(g)(function(_){return _.value0})(e)))))(X(K)(l(C)(w.value))(m(g)(S.create)(i))))))([un(m(g)(function(_){if(_ instanceof Fu)return"Turn on";if(_ instanceof ri)return"Loading...";if(_ instanceof ei)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 123, column 29 - line 126, column 54): "+[_.constructor.name])})(f))]),zr([Wb(X(K)(l(C)(tt(pE)(Lb.value)("true")))(X(K)(l(C)(tt(eD)(Jt.value)("display:none;")))(X(K)(m(g)(function(_){return tt(sE)(Bb.value)(_)})(p))(m(g)(E(tt(eD)(Jt.value)("display:block;")))(p)))))([])])])}}))})}}};var eL=function(){return d.value}(),AE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(I()(Z)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(eL)({periodic:P(Mt(e)(t)(function(n){return l(Ca)(void 0)})(function(n){return function(a){return At(n)([Lt(ct)(.2)([r0(bh)(448)(st())])])}}))})}}};var aL=function(){return d.value}(),kE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(I()(Z)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(aL)({periodic:P(Mt(e)(t)(function(n){return l(Ca)(void 0)})(function(n){return function(a){return At(n)([Lt(ct)(.2)([af(sc)(448)(st())])])}}))})}}};var oL=function(){return d.value}(),gE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(I()(Z)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(oL)({periodic:P(Mt(e)(t)(function(n){return l(Ca)(void 0)})(function(n){return function(a){return At(n)([Lt(ct)(.2)([tp(Qf)(448)(st())])])}}))})}}};var cL=function(){return d.value}(),CE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(I()(Z)({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))(cL)({pan:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return At(n)([c0(dh)(1)([fr(Cr)(a)(st())])])}}))})}}};var lL=function(){return d.value}(),hE=It({reflectType:function(){return`<ul>
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
`}})()()(Z)(lL)({});var pL=function(){return d.value}(),EE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(I()(Z)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(pL)({periodic:P(Mt(e)(t)(function(n){return l(Ca)(void 0)})(function(n){return function(a){return At(n)([Lt(ct)(.2)([im(Ys)(448)(st())])])}}))})}}};var mL=function(){return d.value}(),SE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(I()(I()(Z)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}}))(mL)({code:P(ie(`do
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
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]`)),waveShaper:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){var u=function(o){var i=ic/180;return m(Or)(function(f){var p=jr(f)*2/jr(44100)-1;return(3+o)*p*20*i/(ic+o*qm(La)(xc)(p))})(en(0)(44099))};return At(n)([f0(jh)(Tb(u(400)))([fr(Cr)(a)(st())])])}}))})}}};var DL=function(){return d.value}(),TE=function(t){return function(r){return function(e){return function(n){var a=dt(kt)(r(of.value))(ln),u=Ia(t)(e);return It({reflectType:function(){return`<div>
  <h1>Audio Units</h1>

  <h3>There sure are a lot of them!</h3>
  <p>
    This section provides a tour of the web audio nodes provided by the Web Audio API and, by extension, Ocarina. There are only two omissions:</p>
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
</div>`}})()()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(on()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(I()(Z)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}}))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}}))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}}))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}}))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}}))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}}))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}}))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}}))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}}))(DL)({drumroll:P(vm("\u{1F941}")(n)(u)(function(o){return xt(o)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(o){return function(i){return At(o)([Lt(ct)(1)([fr(Cr)(i)(st())])])}})),toc:P(hE),allpass:P(B0(u)(r)(n)),analyser:P(V0(u)(r)(n)),bandpass:P(G0(u)(r)(n)),constant:P(K0(u)(r)(n)),compression:P(J0(u)(r)(n)),convolution:P(Y0(u)(r)(n)),delay:P(Z0(u)(r)(n)),gain:P(tE(u)(r)(n)),highpass:P(rE(u)(r)(n)),highshelf:P(eE(u)(r)(n)),iirFilter:P(nE(u)(r)(n)),loopBuf:P(aE(u)(r)(n)),lowshelf:P(oE(u)(r)(n)),lowpass:P(uE(u)(r)(n)),notch:P(cE(u)(r)(n)),playBuf:P(_E(u)(r)(n)),peaking:P(fE(u)(r)(n)),microphone:P(iE(u)(r)(n)),pan:P(CE(u)(r)(n)),periodicOsc:P(lE(u)(r)(n)),recorder:P(yE(u)(r)(n)),sawtoothOsc:P(AE(u)(r)(n)),sinOsc:P(kE(u)(r)(n)),squareOsc:P(gE(u)(r)(n)),triangleOsc:P(EE(u)(r)(n)),waveShaper:P(SE(u)(r)(n)),next:Pa(K)(C)(n)(a)})}}}};var qb=function(){function t(){}return t.value=new t,t}(),xE={attr:function(t){return function(r){return b({key:"checked",value:B(r)})}}};var Co=function(){function t(){}return t.value=new t,t}();var ni={attr:function(t){return function(r){return b({key:"type",value:B(r)})}}};var ho=function(t){return function(r){return new F(Q("input")(t)(O()(L(O()(G(j)(r))))))}};var AL=function(t){return t},km=function(t){return function(r){return function(e){return No(t)(X(t.Alternative0().Plus1().Alt0())(l(t.Alternative0().Applicative0())(r))(e))}}};var Dp=function(t){return function(r){return t(r)}},Dc=function(t){return{map:function(r){return function(e){return function(n){return e(m(t)(function(a){return function(u){return a(r(u))}})(n))}}}}},Pi=function(t){return function(r){return function(e){return function(n){return Dp(m(Dc(t))(r)(e))(m(t)(Ec)(n))}}}};var bl=function(t){return Pi(t)(E)};var _u=AL;var FE=function(t){return function(r){return function(e){return _u(function(n){return Ce(t)(X(t.Alternative0().Plus1().Alt0())(l(t.Alternative0().Applicative0())(Dp(r)(n)))(m(t.Filterable1().Functor1())(function(a){return Dp(a)(n)})(e)))})}}},Hb=function(t){return{apply:function(r){return function(e){return function(n){return e(r(m(t)(Ju(ii))(n)))}}},Functor0:function(){return Dc(t)}}};var yl=function(t){return function(r){return xa(function(e){return Be(r)(function(n){return function(){var u=pp(t)();return e({acTime:u,value:n})()}})})}};var OE=function(t){return function(r){return function(e){var n=function(a){return function(u){return function(o){return function(i){return function(f){return function(p){return function(){var s=Jr(o)();return Pn(vt)(s)(function(){var c=pp(t)(),D=Kp(Sg(Mu(La)(u-c-.04)(.01)*1e3))(function(){var et=Jr(o)();return Pn(vt)(et)(function(){return ne(u)(f)(),a(u)(),n(a)(u+p)(o)(i)(f)(p)()})()})();return ne(new S(D))(i)()})()}}}}}}};return xa(function(a){return function(){var o=Br(!0)(),i=Br(w.value)(),f=pp(t)(),p=Br(f+r)();n(a)(r)(o)(i)(p)(r)();var _=Be(e)(function(s){return function(){mt(Zn)(Jr(i))(Ie(vt)(Zr)(i_))();var c=Jr(p)();return n(a)(c+s)(o)(i)(p)(s)()}})();return dt(kt)(dt(kt)(_)(ne(!1)(o)))(mt(Zn)(Jr(i))(Ie(vt)(Zr)(i_)))}})}}};var Ra=function(t){return function(r){return function(e){return function(n){return function(a){var u=e===t||n===r;if(u)return r;var o=(n-r)/(e-t),i=r-o*t;return o*a+i}}}}};var kL=function(){return d.value}(),$E=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<section>
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

</section>`}})()()(I()(I()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}}))(kL)({txt:P(ie(`module Main where

import Prelude

import Control.Alt ((<|>))
import QualifiedDo.OneOfMap as O
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Behavior (sampleBy, sample_, step)
import FRP.Event (memoize)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (fold, mapAccum, sampleOnRight)
import FRP.Event.VBus (V, vbus)
import Data.Number (pi, sin)
import Type.Proxy (Proxy(..))
import Ocarina.Clock (withACTime)
import Ocarina.Control (gain, periodicOsc)
import Ocarina.Interpret (close, constant0Hack, context)
import Ocarina.Math (calcSlope)
import Ocarina.Core (AudioNumeric(..), _linear, bangOn)
import Ocarina.Properties as P
import Ocarina.Run (run2e)

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
        startE = pure unit <|> event.startStop.start
        stopE = event.startStop.stop
        chkState e = step false $ fold (const not) e false
        cbx0 = chkState event.cbx.cbx0
        cbx1 = chkState event.cbx.cbx1
        cbx2 = chkState event.cbx.cbx2
        cbx3 = chkState event.cbx.cbx3
      D.div_
        [ D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                stopE <#> (_ *> push.startStop.start unit)
                startE $> do
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
                            evs f a = sampleOnRight acTime
                              $ map ($)
                              $ sampleOnRight a
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
                                    ( OneOf.do
                                        bangOn
                                        evs ev2 ev3 <#> \\{ f, a, t } -> P.frequency
                                          $ AudioNumeric
                                              { n: 325.6 +
                                                  (calcSlope 1.0 3.0 4.0 15.5 a * sin (pi * f))
                                              , o: t
                                              , t: _linear
                                              }
                                    )
                                ]
                            ]
                    )
                  push.startStop.stop (r *> c0h *> close ctx)
            )
            [ text OneOf.do
                startE $> "Turn on"
                stopE $> "Turn off"
            ]
        , D.div
            ( O.oneOfMap (map (attr D.Style)) O.do
                stopE $> "display:block;"
                startE $> "display:none;"
            )
            ( map
                ( \\e -> D.input
                    ( OneOf.do
                        pure (D.Xtype := "checkbox")
                        pure (D.OnClick := cb (const (e unit)))
                        startE $> (D.Checked := "false")
                    )
                    []
                )
                ([ _.cbx0, _.cbx1, _.cbx2, _.cbx3 ] <@> push.cbx)
            )
        ]
  )`)),empl:P(Wu()(Lu({reflectSymbol:function(){return"cbx"}})()()()(oe({reflectSymbol:function(){return"cbx0"}})()()(oe({reflectSymbol:function(){return"cbx1"}})()()(oe({reflectSymbol:function(){return"cbx2"}})()()(oe({reflectSymbol:function(){return"cbx3"}})()()(Nn)()())()())()())()())(Lu({reflectSymbol:function(){return"startStop"}})()()()(oe({reflectSymbol:function(){return"start"}})()()(oe({reflectSymbol:function(){return"stop"}})()()(Nn)()())()())(Nn)()())()())(d.value)(function(a){return function(u){var o=X(K)(l(C)(void 0))(u.startStop.start),i=function(v){return km(Tr)(!1)(Ru(Tr)(function(c){return function(D){return!c}})(!1)(v))},f=i(u.cbx.cbx3),p=i(u.cbx.cbx2),_=i(u.cbx.cbx1),s=i(u.cbx.cbx0);return zr([Cn(_n(_t)(A)(m(g)(function(){var v=tt(be)(se.value);return function(c){return v(te(E(c)))}}()))([Gr(g)(yt(mn)(Y(g)(o)(nt(at)))(X(K)(l(C)(l(vt)(void 0)))(m(g)(function(v){return v.value0})(n))))(function(v){return function(){v();var D=oa(ce)(),M=Vu(ce)(D)(),et=function(Xr){return function(ve){return function(lt){return Of(Tr)(function(zt){return function(Fr){var Pt=zt.value1+(Fr.value1-zt.value0)*function(){return Fr.value0?Xr:1}();return new rt(new rt(Fr.value1,Pt),Pt)}})(new rt(0,0))(Pi(g)(rt.create)(ve)(lt))}}},Ft=sl(D)(oo(m(g)(function(){var Xr=Mr(Sa)(.04);return function(ve){return Xr(function(lt){return lt.acTime}(ve))}}())(yl(D)(vc)))(function(Xr){var ve=function(le){return function(Vn){return No(Tr)(Xr)(m(g)(sf)(No(Tr)(Vn)(m(g)(function(qe){return function(Ea){return function(ca){return{f:qe,a:Ea,t:ca}}}})(le))))}},lt=m(g)(function(le){return le?4:1})(bl(g)(f)(Xr)),zt=et(4)(p)(Xr),Fr=m(g)(function(le){return le?4:1})(bl(g)(_)(Xr)),Pt=et(8)(s)(Xr);return[Ue(ct)(0)(Gr(g)(ve(Pt)(Fr))(function(le){return En()(Re)({n:Ra(1)(.01)(4)(.15)(le.a)*_s(ic*le.f)+.15,o:le.t,t:Yo})}))([Fi(xi(St(Et()(z(z(ht)(Ti(Ei(ua)))()()()({reflectSymbol:function(){return"spec"}}))(Si)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:325.6,spec:new rt(Pr(wr(ua)()(dn)()(Va))(.3)(Pr(wr(hu)()(bn)()(dn))(-.1)(Pr(wr(Eu)()(yn)()(bn))(.7)(Pr(wr(Su)()(Tu)()(yn))(-.4)(qu)))),Pr(wr(ua)()(dn)()(Va))(.6)(Pr(wr(hu)()(bn)()(dn))(.3)(Pr(wr(Eu)()(yn)()(bn))(.2)(Pr(wr(Su)()(Tu)()(yn))(0)(qu)))))})(Yt(_t)(A)([st(),Gr(g)(ve(zt)(lt))(function(le){return go()(Re)({n:325.6+Ra(1)(3)(4)(15.5)(le.a)*_s(ic*le.f),o:le.t,t:Yo})})]))])]}))(),Wt=dt(kt)(dt(kt)(Ft)(M))(hn(ce)(D));return t(dt(kt)(Wt)(a.startStop.start(void 0)))(),a.startStop.stop(Wt)()}}),Gr(g)(u.startStop.stop)(function(v){return dt(kt)(v)(dt(kt)(t(l(vt)(void 0)))(a.startStop.start(void 0)))})]))([un(Yt(_t)(A)([Y(g)(o)("Turn on"),Y(g)(u.startStop.stop)("Turn off")]))]),xr(_n(_t)(A)(m(g)(tt(bt)(Jt.value)))([Y(g)(u.startStop.stop)("display:block;"),Y(g)(o)("display:none;")]))(m(Or)(function(v){return ho(Yt(_t)(A)([l(C)(tt(ni)(Co.value)("checkbox")),l(C)(tt(be)(se.value)(te(E(v(void 0))))),Y(g)(o)(tt(xE)(qb.value)("false"))]))([])})(Im(Or)([function(v){return v.cbx0},function(v){return v.cbx1},function(v){return v.cbx2},function(v){return v.cbx3}])(a.cbx)))])}}))})}}}};var zb={recip:function(t){return 1/t},Ring0:function(){return xc}};var Vb=function(t){return function(r){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return r}}}};function Al(t){return function(){return function(r){return t(r)()}}}function kl(t){return function(r){return function(e){return function(n){return function(){return n.addEventListener(t,r,e)}}}}}function gl(t){return function(r){return function(e){return function(n){return function(){return n.removeEventListener(t,r,e)}}}}}function Gb(t){return t.clientX}function Jb(t){return t.clientY}function dp(t){return t.button}var bp=Nt("MouseEvent");var wE=function(t){return function(r){return xa(function(e){return Be(r)(function(n){return function(){var u=Jr(t.buttons)();return e({value:n,buttons:u})()}})})}};var ME=function(){var r=Br(w.value)(),e=Br(Ov)(),n=m(N)(FD)(Ci)(),a=Al(function(f){return Ie(vt)(Zr)(function(p){return ne(new S({x:Gb(p),y:Jb(p)}))(r)})(bp(f))})(),u=Al(function(f){return Ie(vt)(Zr)(function(p){return li(ok(Le)(dp(p)))(e)})(bp(f))})(),o=Al(function(f){return Ie(vt)(Zr)(function(p){return li(Xp(Le)(dp(p)))(e)})(bp(f))})();kl(Xe()("mousemove"))(a)(!1)(n)(),kl(Xe()("mousedown"))(u)(!1)(n)(),kl(Xe()("mouseup"))(o)(!1)(n)();var i=function(){return gl(Xe()("mousemove"))(a)(!1)(n)(),gl(Xe()("mousedown"))(u)(!1)(n)(),gl(Xe()("mouseup"))(o)(!1)(n)()};return{position:r,buttons:e,dispose:i}},PE=xa(function(t){return function(){var e=m(N)(FD)(Ci)(),n=Al(function(a){return Ie(vt)(Zr)(function(u){return t(dp(u))})(bp(a))})();return kl(Xe()("mousedown"))(n)(!1)(e)(),gl(Xe()("mousedown"))(n)(!1)(e)}});var RE=function(t){return _u(function(r){return m(g)(function(e){return e.value(e.buttons)})(wE(t)(r))})};var Xb=function(t){return t};function hm(){return Date.now()}var iS=function(t){return xa(function(r){return Be(t)(function(e){return function(){var a=hm();return r({time:a,value:e})()}})})};var n1=_u(function(t){return m(g)(function(r){return r.value(r.time)})(iS(t))}),Yb=m(Dc(g))(function(){var t=ap(C0);return function(r){return t(Xb(r))}}())(n1);var u1=function(t){var r=function(u){return function(o){return function(i){return function(f){return function(p){return function(_){return function(s){var v=Mr(o.DivisionRing1().Ring0().Semiring0())(pa(o.DivisionRing1().Ring0().Semiring0()))(pa(o.DivisionRing1().Ring0().Semiring0())),c=function(D){return function(M){if(D.last instanceof w)return M;if(D.last instanceof S)return Mr(i)(M)(f(function(et){return Yu(o.EuclideanRing0())(Sn(o.DivisionRing1().Ring0().Semiring0())(et(Mr(i)(D.last.value0.value1)(D.now.value1)))(mu(o.DivisionRing1().Ring0())(D.now.value0)(D.last.value0.value0)))(v)}));throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 106, column 5 - line 106, column 35): "+[D.constructor.name,M.constructor.name])}};return _u(function(D){var M=Dp(s)(Y(u.Filterable1().Functor1())(D)(nt(at))),et=ts(u)(Pi(u.Filterable1().Functor1())(rt.create)(_)(M)),Ft=Ru(u)(yr(c))(p)(et);return No(u)(Ft)(D)})}}}}}}},e=function(u){return function(o){return r(u)(o)(o.DivisionRing1().Ring0().Semiring0())(function(i){return i(nt(at))})}},n=function(u){return function(o){return _u(function(i){return No(Tr)(Ff(Tr)(function(f){var p=o(km(Tr)(u)(f));return bl(g)(p)(i)}))(i)})}},a=function(u){return function(o){return function(i){if(uk(u))return-8*(o-1)-i*2;if(re)return 2*(4-o);throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 62, column 3 - line 64, column 34): "+[u.constructor.name,o.constructor.name,i.constructor.name])}}};return n(2)(function(u){return e(Tr)(Vb(Nl)(zb))(2)(m(Dc(g))(ae())(Yb))(function(){var o=n(10)(function(i){return e(Tr)(Vb(Nl)(zb))(10)(m(Dc(g))(ae())(Yb))(yt(Hb(g))(yt(Hb(g))(m(Dc(g))(a)(RE(t)))(u))(i))});return FE(Tr)(o)(Y(g)(PE)(o))}())})},o1=function(){return d.value}(),cS=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in ocarina that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(I()(I()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}}))(o1)({txt:P(ie(`module Main

import Prelude

import Control.Alt ((<|>))
import QualifiedDo.OneOfMap as O
import QualifiedDo.Alt as OneOf
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
import FRP.Event.Class (class IsEvent, fix, fold, sampleOnRight, withLast)
import FRP.Event.Mouse (Mouse, down, getMouse)
import FRP.Event.VBus (V, vbus)
import Test.QuickCheck (arbitrary, mkSeed)
import Test.QuickCheck.Gen (evalGen)
import Type.Proxy (Proxy(..))
import Ocarina.Clock (withACTime)
import Ocarina.Control (bandpass_, gain, lowpass_, periodicOsc, squareOsc_)
import Ocarina.Interpret (close, constant0Hack, context)
import Ocarina.Core (AudioNumeric(..), _linear, bangOn)
import Ocarina.Properties as P
import Ocarina.Run (run2e)

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
        { input: sample_ b s, output: sampleOnRight event s }

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
        sampleOnRight z e
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
        startE = pure unit <|> event.start
        stopE = event.stop
      D.div_
        [ D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                event.stop <#> (_ *> push.start unit)
                startE $>
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
            )
            [ text $ OneOf.do
                startE $> "Turn on"
                stopE $> "Turn off"
            ]
        ]
  )`)),empl:P(Wu()(oe({reflectSymbol:function(){return"start"}})()()(oe({reflectSymbol:function(){return"stop"}})()()(Nn)()())()())(d.value)(function(a){return function(u){var o=X(K)(l(C)(void 0))(u.start);return zr([Cn(_n(_t)(A)(m(g)(function(){var i=tt(be)(se.value);return function(f){return i(te(E(f)))}}()))([Gr(g)(yt(mn)(Y(g)(o)(nt(at)))(X(K)(l(C)(l(vt)(void 0)))(m(g)(function(i){return i.value0})(n))))(function(i){return function(){i();var p=oa(ce)(),_=Vu(ce)(p)(),s=ME(),v=S_(0)(1e4)(),c=function(lt){return{o:lt.value0+.04,n:lt.value1,t:Yo}},D=m(fo)(function(lt){return lt-.5})(jc(rC)),M=mt(Gc)(D)(function(lt){return mt(Gc)(D)(function(zt){return mt(Gc)(D)(function(Fr){return mt(Gc)(D)(function(Pt){return l(B_)(Pr(wr(ua)()(dn)()(Va))(lt)(Pr(wr(hu)()(bn)()(dn))(zt)(Pr(wr(Eu)()(yn)()(bn))(Fr)(Pr(wr(Su)()(Tu)()(yn))(Pt)(qu)))))})})})}),et=yt(Jc)(m(fo)(rt.create)(M))(M),Ft=yt(Jc)(yt(Jc)(yt(Jc)(m(fo)(function(lt){return function(zt){return function(Fr){return function(Pt){return{s0:lt,s1:zt,s2:Fr,s3:Pt}}}}})(et))(et))(et))(et),Wt=pc(Ft)({newSeed:cc(v),size:5}),Xr=sl(p)(oo(m(g)(function(lt){return new rt(lt.acTime,lt.value)})(yl(p)(bl(g)(u1(s))(vc))))(function(lt){return[Ue(ct)(0)(m(g)(function(){var zt=En()(Re),Fr=qn(An)(function(Pt){return Mu(La)(-.4)(.5*(Pt-1))});return function(Pt){return zt(c(Fr(Pt)))}}())(lt))([ul(sd(St(Et()(z(z(ht)(Sh)()()()({reflectSymbol:function(){return"q"}}))(rd)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:90.4,q:20})([n0(Qf)(90.4)])]),Ue(ct)(0)(m(g)(function(){var zt=En()(Re),Fr=qn(An)(function(Pt){return Mu(La)(-.2)(.4*(Pt-3))});return function(Pt){return zt(c(Fr(Pt)))}}())(lt))([gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:90.4*4,q:20})([Fi(xi(St(Et()(z(z(ht)(Ti(Ei(ua)))()()()({reflectSymbol:function(){return"spec"}}))(Si)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:90.4*3.02,spec:Wt.s0})(X(K)(st())(m(g)(function(){var zt=go()(Re),Fr=qn(An)(function(Pt){return 90.4*3.02+14*(Pt-1)});return function(Pt){return zt(c(Fr(Pt)))}}())(lt)))])]),Ue(ct)(0)(m(g)(function(){var zt=En()(Re),Fr=qn(An)(function(Pt){return Mu(La)(-.1)(.2*(Pt-6))});return function(Pt){return zt(c(Fr(Pt)))}}())(lt))([gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:90.4*6,q:20})([Fi(xi(St(Et()(z(z(ht)(Ti(Ei(ua)))()()()({reflectSymbol:function(){return"spec"}}))(Si)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:90.4*5.07,spec:Wt.s1})(X(K)(st())(m(g)(function(){var zt=go()(Re),Fr=qn(An)(function(Pt){return 90.4*5.07+18*(Pt-1)});return function(Pt){return zt(c(Fr(Pt)))}}())(lt)))])]),Ue(ct)(0)(m(g)(function(){var zt=En()(Re),Fr=qn(An)(function(Pt){return Mu(La)(0)(.2*(Pt-3))});return function(Pt){return zt(c(Fr(Pt)))}}())(lt))([gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:90.4*8,q:20})([Fi(xi(St(Et()(z(z(ht)(Ti(Ei(ua)))()()()({reflectSymbol:function(){return"spec"}}))(Si)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:90.4*7.13,spec:Wt.s2})(X(K)(st())(m(g)(function(){var zt=go()(Re),Fr=qn(An)(function(Pt){return 90.4*7.13+32*(Pt-1)});return function(Pt){return zt(c(Fr(Pt)))}}())(lt)))])]),Ue(ct)(0)(m(g)(function(){var zt=En()(Re),Fr=qn(An)(function(Pt){return Mu(La)(0)(.1*(Pt-7))});return function(Pt){return zt(c(Fr(Pt)))}}())(lt))([Fi(xi(St(Et()(z(z(ht)(Ti(Ei(ua)))()()()({reflectSymbol:function(){return"spec"}}))(Si)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:90.4*9.14,spec:Wt.s3})(X(K)(st())(m(g)(function(){var zt=go()(Re),Fr=qn(An)(function(Pt){return 90.4*9.14+31*(Pt-1)});return function(Pt){return zt(c(Fr(Pt)))}}())(lt)))])]}))(),ve=dt(kt)(dt(kt)(Xr)(_))(hn(ce)(p));return t(dt(kt)(ve)(a.start(void 0)))(),a.stop(ve)()}}),Gr(g)(u.stop)(function(i){return dt(kt)(i)(dt(kt)(t(l(vt)(void 0)))(a.start(void 0)))})]))([un(Yt(_t)(A)([Y(g)(o)("Turn on"),Y(g)(u.stop)("Turn off")]))])])}}))})}}}};var c1=function(){return d.value}(),fS=function(t){return function(r){return function(e){return function(n){var a=Ia(t)(e);return It({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(I()(I()(on()(Z)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}}))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}}))(c1)({next:Pa(K)(C)(n)(dt(kt)(r(lp.value))(ln)),fold:P($E(a)(r)(e)(n)),fix:P(cS(a)(r)(e)(n))})}}}};var l1=function(){function t(){}return t.value=new t,t}(),lS=function(){function t(){}return t.value=new t,t}(),Zb=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),_1=`module Main where

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

import Math (pow)
import Ocarina.Control (gain_, gain, sinOsc)
import Ocarina.Core (AudioEnvelope(..), AudioOnOff(..), _on, _off)
import Ocarina.Properties (onOff)
import Ocarina.Properties as P
import Ocarina.Run (run2_)

data UIEvents = Init | Start | Stop (Effect Unit)

-- an event to turn our oscillators on
oon o = pure $ onOff $ AudioOnOff { x: _on, o }
-- an event to turn our oscillators off
oof o = pure $ onOff $ AudioOnOff { x: _off, o }
-- an event with an envelope for our gain
env o = pure $ P.gain
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
main = runInBody1 (bus \\push -> lcmap (pure Init <|> _) \\event ->
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
`;var p1=function(){return d.value}(),s1=function(t){return function(r){return function(e){return l(t)(ff(r)(Zc)({x:ZD,o:e}))}}},m1=function(t){return function(r){return function(e){return l(t)(ff(r)(Zc)({x:mh,o:e}))}}},v1=Fa(xn)(jr)(function(t){var r=function(a){return X(K)(s1(C)()(a+.27*(t*oc(1.005)(t))))(m1(C)()(a+3+.3*(t*oc(1.005)(t))))},e=function(a){return l(C)(En()(On)({p:[0,.4,.1,.05,.01,0],o:a+.3*(t*oc(1.005)(t)),d:.8}))},n=function(a){return function(u){return Ue(ct)(0)(e(a))([af(sc)(200+t*u)(r(a))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),_S=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(I()(I()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}))(d.value)(p1)({txt:P(ie(_1)),ex0:P(Ke(function(n){return Fa(xn)(function(a){return X(K)(l(C)(l1.value))(a)})(function(a){return zr([Cn(Gr(g)(yt(mn)(m(g)(rt.create)(a))(X(K)(l(C)(l(vt)(void 0)))(m(g)(function(u){return u.value0})(e))))(function(u){return tt(be)(se.value)(te(E(function(){return u.value0 instanceof Zb?dt(kt)(dt(kt)(u.value0.value0)(n(lS.value)))(t(l(vt)(void 0))):function(){u.value1();var i=Dm([Lt(ct)(1)(So(Eo)(m(Or)(v1)(en(0)(100))))])();return t(dt(kt)(i)(n(lS.value)))(),n(new Zb(i))()}}())))}))([un(Gr(g)(a)(function(u){return u instanceof Zb?"Turn off":"Turn on"}))])])})}))})}}};var Ii=function(){function t(){}return t.value=new t,t}();var bc={attr:function(t){return function(r){return b({key:"max",value:B(r)})}}};var Ri=function(){function t(){}return t.value=new t,t}();var yc={attr:function(t){return function(r){return b({key:"min",value:B(r)})}}};var Ni=function(){function t(){}return t.value=new t,t}();var Ac={attr:function(t){return function(r){return b({key:"input",value:ft(r)})}}};var Li=function(){function t(){}return t.value=new t,t}(),kc={attr:function(t){return function(r){return b({key:"step",value:B(r)})}}};var Bi=function(){function t(){}return t.value=new t,t}();var gc={attr:function(t){return function(r){return b({key:"value",value:B(r)})}}};var ai=function(t){return function(r){return function(e){return X(t)(r)(e(void 0))}}};var d1=Ig,pu={convert:function(t){return t}},yp={convert:function(t){return $_(t)}},sS=function(t){return t},ty=function(t){return t.convert},ja=function(t){return function(r){return function(e){return Dt(d1)($_(r))(ty(t)(e(void 0)))}}};var Ap=function(t){return function(r){return function(e){return function(n){return _n(Rg)(r)(e)(sS(ty(t)(n)))}}}};function vS(t){return t.target}var Cl=function(t){return Ye(vS(t))};var A1=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable)
import Bolson.Core (envy)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create)
import FRP.Event.Class (biSampleOn)
import FRP.Event.VBus (V, vbus)
import QualifiedDo.Alt as OneOf
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import Ocarina.Control (loopBuf)
import Ocarina.Core (bangOn)
import Ocarina.Interpret (bracketCtx, decodeAudioDataFromUri)
import Ocarina.Math (calcSlope)
import Ocarina.Properties (loopEnd, loopStart, playbackRate)
import Ocarina.Run (run2_)
import Ocarina.WebAPI (BrowserAudioBuffer)
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
    -> Domable Effect lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ pure $ envy $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        sl0 = event.slider.s0
        sl1 = event.slider.s1
        sl2 = event.slider.s2
        start = event.startStop.start <|> pure unit
        music = run2_
          [ loopBuf
              { buffer: buffer
              , playbackRate: 2.6
              , loopStart: 0.6
              , loopEnd: 1.1
              }
              OneOf.do
                bangOn
                map
                  (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate)
                  sl0
                map
                  (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart)
                  sl1
                map
                  (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd)
                  (biSampleOn sl2 (add <$> (pure 0.0 <|> sl1)))
          ]
      D.div_
        $
          map
            ( \\{ l, f } -> D.div_
                [ text_ l
                , D.input
                    ( O.oneOfMap pure O.do
                        D.Xtype := "range"
                        D.Min := "0"
                        D.Max := "100"
                        D.Step := "1"
                        D.Value := "50"
                        D.OnInput := cb
                          ( traverse_
                              (valueAsNumber >=> f)
                              <<< (=<<) fromEventTarget
                              <<< target
                          )
                    )
                    []
                ]
            )
            [ { l: "Playback rate", f: push.slider.s0 }
            , { l: "Loop start", f: push.slider.s1 }
            , { l: "Loop end", f: push.slider.s2 }
            ] <>
            [ D.button
                ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                    start $> (music >>= push.startStop.stop)
                    event.startStop.stop <#>
                      (_ *> push.startStop.start unit)
                )
                [ text OneOf.do
                    start $> "Turn on"
                    event.startStop.stop $> "Turn off"
                ]
            ]
`,k1=function(){return d.value}(),g1="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",DS=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
 <h2>Example 2: Three sliders</h2>

  <p>In this example, we'll use three sliders to control the playback rate, the start time, and the end time of a looping buffer.</p>

  <p>There is a fair bit of DOM-related code in this example, so before showing the whole thing, let's isolate the Ocarina bit.</p>

  <pre><code>@wagtxt@</code></pre>

  <p>Note that our loopBuf consumes four events: in addition to the three sliders, there is a <code>bangOn</code> event that turns it on. For the events belonging to range sliders, we use <code>calcSlope</code> to normalize the range to sensible values for these parameters.</p>

  <p>Because each slider event contains a number, we can compose it with a function from <code>Ocarina.Properties</code>, like <code>playbackRate</code> or <code>loopStart</code>, to create an event that controls a Ocarina parameter. The <code>oneOf</code> directive indicates that the incoming event will be "one of" the events in the array. It's also possible to use the tie-fighter, aka <code>alt</code>, to separate each event, but I like the array syntax when possible as tie fighters do, after all, work for the Empire, and who likes the Empire?</p>

  <p>And below you'll find the full example. It also shows useful patterns like downloading audio files and filtering events.</p>

  <pre><code>@txt@</code></pre>

  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(I()(I()(I()(Z)({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}}))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(d.value)(k1)({wagtxt:P(ie(`run2_
  $ loopBuf
      { buffer: buffer
      , playbackRate: 2.6
      , loopStart: 0.6
      , loopEnd: 1.1
      }
  $ OneOf.do
      bangOn
      (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate) <$> sl0
      (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart) <$> sl1
      (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd) <$> biSampleOn sl2
          (add <$> (pure 0.0 <|> sl1))`)),txt:P(ie(A1)),ex1:P(Wu()(Lu({reflectSymbol:function(){return"slider"}})()()()(oe({reflectSymbol:function(){return"s0"}})()()(oe({reflectSymbol:function(){return"s1"}})()()(oe({reflectSymbol:function(){return"s2"}})()()(Nn)()())()())()())(Lu({reflectSymbol:function(){return"startStop"}})()()()(oe({reflectSymbol:function(){return"loading"}})()()(oe({reflectSymbol:function(){return"start"}})()()(oe({reflectSymbol:function(){return"stop"}})()()(Nn)()())()())()())(Nn)()())()())(d.value)(function(n){return function(a){var u=X(K)(a.startStop.start)(l(C)(void 0)),o=function(i){return fr(ef(St(Et()(z(z(z(z(ht)(rl)()()()({reflectSymbol:function(){return"playbackRate"}}))(K_)()()()({reflectSymbol:function(){return"loopStart"}}))(X_)()()()({reflectSymbol:function(){return"loopEnd"}}))(rf)()()()({reflectSymbol:function(){return"buffer"}})))(Ct()())))({buffer:i,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(ai(K)(st())(function(){return ai(K)(m(g)(function(){var f=ia()(Ks),p=Ra(0)(.2)(100)(5);return function(_){return f(p(_))}}())(a.slider.s0))(function(){return ai(K)(m(g)(function(){var f=Q0(),p=Ra(0)(0)(100)(1.2);return function(_){return f(p(_))}}())(a.slider.s1))(function(){return m(g)(function(){var f=X0(),p=Ra(0)(.05)(100)(1);return function(_){return f(p(_))}}())(function(f){return yt(mn)(f)(a.slider.s2)}(m(g)(Mr(Sa))(X(K)(l(C)(0))(a.slider.s1))))})})}))};return zr(Dt(Je)(m(Or)(function(i){return zr([ie(i.l),ho(Ap(pu)(A)(l(C))(ja(pu)(tt(ni)(Co.value)("range"))(function(){return ja(pu)(tt(yc)(Ri.value)("0"))(function(){return ja(pu)(tt(bc)(Ii.value)("100"))(function(){return ja(pu)(tt(kc)(Li.value)("1"))(function(){return ja(yp)(tt(gc)(Bi.value)("50"))(function(){return tt(Ac)(Ni.value)(te(function(){var f=Ie(vt)(Zr)(mf(Zn)(Xc)(i.f)),p=jn(ma)(Hf);return function(_){return f(p(Cl(_)))}}()))})})})})})))([])])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([Cn(Ap(pu)(A)(m(g)(function(){var i=tt(be)(se.value);return function(f){return i(te(E(f)))}}()))(ja(pu)(Y(g)(a.startStop.loading)(l(vt)(void 0)))(function(){return ja(yp)(Gr(g)(a.startStop.stop)(function(i){return dt(kt)(i)(dt(kt)(t(l(vt)(void 0)))(n.startStop.start(void 0)))}))(function(){return Gr(g)(yt(mn)(Y(g)(u)(nt(at)))(X(K)(l(C)(l(vt)(void 0)))(m(g)(function(i){return i.value0})(e))))(function(i){return function(){i(),n.startStop.loading(void 0)();var p=ko(mt(Pe)(oa(Me))(function(_){return mt(Pe)(Vu(Me)(_))(function(s){return mt(Pe)(xt(_)(g1))(function(v){return ye(Me)(function(){var D=At(_)([o(v)])(),M=dt(kt)(dt(kt)(D)(s))(hn(ce)(_));return n.startStop.stop(M)(),M})})})}))();return t(function(){return n.startStop.start(void 0)(),ti(wi(p))()})(),void 0}})})})))([un(ai(K)(m(g)(E("Turn off"))(a.startStop.stop))(function(){return m(g)(E("Turn on"))(u)}))])]))}}))})}}};var h1=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import QualifiedDo.OneOfMap as O
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (Event, makeEvent, memoize, subscribe)

import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import Ocarina.Clock (interval)
import Ocarina.Control (bandpass_, fan1, gain, gain_, highpass_, triangleOsc)
import Ocarina.Core (Audible, AudioEnvelope(AudioEnvelope), bangOn)
import Ocarina.Interpret (close, context)
import Ocarina.Math (calcSlope)
import Ocarina.Properties (frequency)
import Ocarina.Properties as P
import Ocarina.Run (run2e)
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
        start = event.startStop.start <|> pure unit

        music :: forall lock. _ -> Event (Array (Audible _ lock _))
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
                ( O.oneOfMap pure O.do
                    D.Xtype := "range"
                    D.Min := "0"
                    D.Max := "100"
                    D.Step := "1"
                    D.Value := "50"
                    D.OnInput := cb
                      ( traverse_
                          (valueAsNumber >=> push.slider)
                          <<< (=<<) fromEventTarget
                          <<< target
                      )
                )
                []
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
  )`,E1=_u(function(t){return xa(function(r){return Be(t)(function(e){return function(){var a=Jo();return r(e(a))()}})})}),S1=function(){return d.value}(),T1=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(re)return 587.329536;throw new Error("Failed pattern match at Ocarina.Example.Docs.Events.Ex2 (line 224, column 1 - line 224, column 23): "+[t.constructor.name])},dS=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
  <h2>Example 3: Fascinating rhyhtm</h2>

  <p>Ocarina comes with several different ways to hook into the Web Audio API's sample-accurate timers. In this section, we'll use a Ocarina <code>interval</code> event to create a sample-accurate ticker. We'll also use a <code>random</code> beahvior to change up our samples.</p>

  <p><code>interval :: AudioContext -> Event Number -> Event Number</code> in ocarina is similar to <a href=""><code>interval :: Int -> Event Instant</code></a> from the <code>Event</code> library with a few important exceptions.</p>

  <ul>
    <li>The ocarina interval works in seconds (<code>Number</code>) instead of milliseconds.</li>
    <li>The ocarina interval needs an audio context to work.</li>
    <li>The ocarina interval gets its timing from an <code>Event Number</code> instead of a plain old <code>Number</code>. This is necessary to have variable rates.</li>
  </ul>

  <blockquote><code>interval</code> works fine for a stream of events where each event is separated by more than ~100 milliseconds. For anything faster, you'll likely want to use <code>requestAnimationLoop</code> coupled with a local state, as it will be more efficient for older and battery-sensitive devices.</blockquote>

  <p>In the following example, we use <code>interval</code> to control the playback rate of an analogue synth. We'll also use a custom behavior called <code>random</code> to control the pitch.</p>

  <p>One important optimization we make here is the use of the function <code>memoize</code>. Whenever we're dealing with audio-clock timing, we want to limit the number of subscriptions to receive events from the audio clock. Ideally, there is only one subscription that takes a reading of the clock as a single source of truth. Because we are in PureScript-land, events (like everything else), are referrentially transparent, meaning that new ones will get created every time you use them (just like a new <code>2</code> is created every time you type the value <code>2</code>: they don't all refer to one uber-<code>2</code>). To sync all the events to the <i>same</i> source, we use <code>memoize</code>. While this optimization is not necessary, I recommend it: it will make sure the timing is 100% accurate at a very small energy cost (meaning <code>memoize</code> will eat up slightly more power from a phone's battery, but still not much).</p>

  <pre><code>@txt@</code></pre>

  @ex2@

</section>
`}})({reflectType:function(){return"@"}})()()(I()(I()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))(d.value)(S1)({txt:P(ie(h1)),ex2:P(Wu()(oe({reflectSymbol:function(){return"slider"}})()()(Lu({reflectSymbol:function(){return"startStop"}})()()()(oe({reflectSymbol:function(){return"start"}})()()(oe({reflectSymbol:function(){return"stop"}})()()(Nn)()())()())(Nn)()())()())(d.value)(function(n){return function(a){var u=X(K)(a.startStop.start)(l(C)(void 0)),o=function(i){return oo(i)(function(f){var p=m(g)(function(){var M=Mr(Sa)(.01);return function(et){return M(Qe(et))}}())(f),_=m(g)(qa)(f),s=X(K)(st())(m(g)(function(){var M=go()(Ks);return function(et){return M(T1(et))}}())(_)),v=m(g)(function(M){return Qs(function(et){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:et}}(M))})(p),c=m(g)(function(M){return Qs(function(et){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:et}}(M))})(p),D=m(g)(function(M){return Qs(function(et){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:et}}(M))})(p);return[wa(im(Ys)(0)(s))(function(M){return function(et){return Lt(ct)(2)([Ue(ct)(0)(m(g)(En()(On))(D))([gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:1e3,q:20})([M])]),Ue(ct)(0)(m(g)(En()(On))(c))([gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:2e3,q:20})([M])]),Ue(ct)(0)(m(g)(En()(On))(v))([al(md(St(Et()(z(z(ht)(Fh)()()()({reflectSymbol:function(){return"q"}}))(ed)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:4e3,q:20})([M])])])}})]})};return zr([zr([ie("tempo"),ho(Ap(pu)(A)(l(C))(ja(pu)(tt(ni)(Co.value)("range"))(function(){return ja(pu)(tt(yc)(Ri.value)("0"))(function(){return ja(pu)(tt(bc)(Ii.value)("100"))(function(){return ja(pu)(tt(kc)(Li.value)("1"))(function(){return ja(yp)(tt(gc)(Bi.value)("50"))(function(){return tt(Ac)(Ni.value)(te(function(){var i=Ie(vt)(Zr)(mf(Zn)(Xc)(n.slider)),f=jn(ma)(Hf);return function(p){return i(f(Cl(p)))}}()))})})})})})))([])]),Cn(_n(_t)(A)(m(g)(function(){var i=tt(be)(se.value);return function(f){return i(te(E(f)))}}()))([Gr(g)(yt(mn)(Y(g)(u)(nt(at)))(X(K)(l(C)(l(vt)(void 0)))(m(g)(function(i){return i.value0})(e))))(function(i){return function(){i();var p=oa(ce)(),_=Pi(g)(rt.create)(E1)(OE(p)(.91)(m(g)(Ra(0)(.42)(100)(1.4))(a.slider))),s=sl(p)(o(_))(),v=dt(kt)(s)(hn(ce)(p));return t(dt(kt)(v)(n.startStop.start(void 0)))(),n.startStop.stop(dt(kt)(v)(hn(ce)(p)))()}}),Gr(g)(a.startStop.stop)(function(i){return dt(kt)(i)(dt(kt)(t(l(vt)(void 0)))(n.startStop.start(void 0)))})]))([un(Yt(_t)(A)([Y(g)(u)("Turn on"),Y(g)(a.startStop.stop)("Turn off")]))])])}}))})}}};var F1=function(){return d.value}(),bS=function(){return pr({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(Z)(d.value)(F1)({})}();var $1=function(){return d.value}(),yS=function(){return pr({reflectType:function(){return`<section>
  <h2>Events in Ocarina</h2>
  <p>Ocarina follows a consistent pattern: every audio unit accepts an event containing a <code>newtype</code> around a <code>Variant</code> of parameters that can be changed. As a motivating example, let's look at the definition of <code>sinOsc</code>.</p>

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

  <p>In practice, you'll never need to use newtypes presented above. The <code>Ocarina.Properties</code> module has highly-overloaded smart constructors for all these values. For example, to use an <code>Event Number</code> to set the frequency of a <code>sinOsc</code>, you'd write <code>frequency &lt;$&gt; event</code> instead of <code>SinOsc &lt;&lt;&lt; inj (Proxy :: _ "frequency") &lt;&lt;&lt; AudioParameter &lt;&lt;&lt; { o: 0.0, t: _linearRamp, n: _ } &lt;$&gt; event</code>. Of course, you <i>can</i> write the longer format, but life is short and keystrokes are precious! Below is a table showing the varoius smart constructors available and the units to which they apply.</p>


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

  <p>You can use this smart-constructor pattern to transform any <code>Event</code> into something that Ocarina can consume. For example:</p>

  <ul>
    <li>If you have an <code>Event Number</code> called <code>myFreq</code> and you'd like it to control the frequency of a band-pass filter, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq)</code>.</li>
    <li>If you have an <code>Event Number</code> called <code>myQ</code> and you'd like it to control the Q value of the same bandpass, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq &lt;|&gt; q &lt;$&gt; myQ)</code> <i>or</i> <code>bandpass 440.0 $ oneOf [frequency &lt;$&gt; myFreq, q &lt;$&gt; myQ]</code>.</li>
    <li>If you'd like <code>myFreq</code> <i>only</i> to have an effect when it's over <code>1000.0</code>, you can write <code>bandpass 440.0 (frequency &lt;$&gt; filter (_ > 1000.0) myFreq &lt;|&gt; q &lt;$&gt; myQ)</code>.</li>
  </ul>

  <p>None of these transformations are unique to Ocarina:</p>
  <ul>
    <li>Because <code>Event</code> implements <a href=""><code>Functor</code></a>, you can use <code>map</code> (aka <code>&lt;$&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Alt</code></a>, you can use <code>alt</code> (aka <code>&lt;|&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Plus</code></a>, you can use <code>empty</code> for an event that emits nothing as well as <code>oneOf</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>Filterable</code></a>, you can use <code>filter</code>, <code>filterMap</code>, <code>partition</code>, <code>partitionMap</code>, and <code>compact</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>IsEvent</code></a>, you can use <code>bang</code> to emit something right away, <code>sampleOn</code> to sample one event's most recent value based on another event, and all of the other functions rolled into <a href=""><code>IsEvent</code></a>.</li>
  </ul>

  <p>
    This gets to one of the core design principles of Ocarina. Idiomatic Ocarina projects use functional reactive programming as a way to "steer" web audio, and Ocarina aims to be a minimal viable framework to shepherd events to their web-audio destinations.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(Z)(d.value)($1)({})}();var M1=function(){return d.value}(),AS=function(){return pr({reflectType:function(){return`<section>

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
    When using Ocarina, you have to get your events from somewhere. At a minimum, you'll consume a browser interaction like a click or swipe that turns on the audio. In fact, without some form of human interaction, most browsers will block the Web Audio API from turning on.
  </p>
  <p>
    <code>Events</code> are often produced within a web framework like <a href="https://github.com/mikesol/purescript-deku">Deku</a>, Halogen or React. They don't have to be, though - you can create and consume your own events.
  </p>

  <h3>Behavior</h3>

  <p>
    The <code>Behavior</code> type takes an event that needs to be "unlocked" (meaning in the form of <code>a -> b</code>, so an <code>a</code> is needed to unlock a <code>b</code>) and unlocks it with an <code>a</code>. Behaviors don't need to produce their <code>a</code> immediately. In fact, they don't need to produce it at all: it's entirely possible to create <code>Behavior (const empty)</code> that "mutes" the event. This resembles the physical world: when we want to observe a behavior, like the weather outside or the axial rotation of the Earth, there is a time-cost to observing anything that ranges from instantaneous to infinite.
  </p>

  <p>
    In Ocarina, we usually want to observe the behavior of things like a mouse's position, an audio buffer's content or a random number generator.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(Z)(d.value)(M1)({})}();var I1=function(){return d.value}(),kS=function(t){return function(r){return function(e){return function(n){var a=function(o){return Pa(K)(C)(n)(dt(kt)(r(o))(ln))},u=Ia(t)(e);return pr({reflectType:function(){return`<div>
  <h1>Events</h1>

  <h3>Clicks, wiggles and loops, oh my!</h3>
  <p>
    The true magic of web audio lies in its ability to harness the rich interactivity built into the browser. We can use mouse clicks, finger swipes and animation loops to create beautiful audio landscapes. But how can we tame the complexity of all these events in an expressive, declarative, functional manner? Enter <code>Event</code>, the abstraction that allows us to build rich reactive works using Ocarina.
  </p>

  @primer@
  @inOcarina@
  @flavors@
  @ex0@
  @ex1@
  @ex2@

  <h2>Next steps</h2>
  <p>In this section, saw how to build rich audio applications using the <code>Event</code> and <code>Behavior</code> types. We also covered the three most common patterns you'll see when working with events: events that need to happen <i>now</i>, events that come from user interaction, and timed events. In the next section, we'll look at different ways to specify <a @next@ style="cursor:pointer;">the numeric parameters being sent as events</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(I()(I()(I()(I()(I()(on()(I()(Z)({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"inOcarina"}})({reflectSymbol:function(){return"inOcarina"}}))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}))(d.value)(I1)({next:a(cp.value),primer:P(AS),inOcarina:P(yS),flavors:P(bS),ex0:P(_S(u)(r)(n)),ex1:P(DS(u)(r)(n)),ex2:P(dS(u)(r)(n))})}}}};var N1=function(){return d.value}(),gS=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0 do
      let ooo n = pure $ onOff $ dt (add n) apOn
      [ playBuf tink0 (ooo 0.1)
      , playBuf tink1 (ooo 0.2)
      , playBuf tink2 (ooo 0.9)
      , playBuf tink3 (ooo 1.8)
      ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(I()(Z)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(N1)({ai0:P(Mt(e)(t)(function(n){return Ao(wn)(yt($i)(yt($i)(yt($i)(m(ll)(function(a){return function(u){return function(o){return function(i){return{tink0:a,tink1:u,tink2:o,tink3:i}}}}})($n(wn)(xt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))($n(wn)(xt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))($n(wn)(xt(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))($n(wn)(xt(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return At(n)([Lt(ct)(1)(function(){var u=function(o){return l(C)(ff()(Zc)(Xs()(Mr(Sa)(o))(J_)))};return[zn(Ga)(a.tink0)(u(.1)),zn(Ga)(a.tink1)(u(.2)),zn(Ga)(a.tink2)(u(.9)),zn(Ga)(a.tink3)(u(1.8))]}())])}}))})}}};var B1=function(){return d.value}(),CS=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0
      $ do
          let
            ooo n = pure $ onOff $ dt (add n) apOn
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
`}})({reflectType:function(){return"@"}})()()(I()(Z)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(B1)({ai0:P(Mt(e)(t)(function(n){return Ao(wn)(yt($i)(yt($i)(yt($i)(m(ll)(function(a){return function(u){return function(o){return function(i){return{tink0:a,tink1:u,tink2:o,tink3:i}}}}})($n(wn)(xt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))($n(wn)(xt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))($n(wn)(xt(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))($n(wn)(xt(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return At(n)([Lt(ct)(1)(function(){var u=function(i){return l(C)(ff()(Zc)(Xs()(Mr(Sa)(i))(J_)))},o=function(i){var f=Za(Ku)(i)(4);return f===0?a.tink0:f===1?a.tink1:f===2?a.tink2:a.tink3};return Gr(Or)(en(0)(100))(function(i){var f=jr(i);return zn(Ga)(o(i))(u(.3+.3*(f*oc(1.005)(f))))})}())])}}))})}}};var U1=function(){return d.value}(),hS=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(I()(Z)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(U1)({ai0:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([wa(fr(Cr)(a)(st()))(function(u){return function(o){return Lt(ct)(.8)([gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:400,q:1})([u]),gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:880,q:5})([u]),gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:1200,q:10})([u]),gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:2e3,q:20})([u]),gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:3e3,q:30})([u])])}})])}}))})}}};var H1=function(){return d.value}(),ES=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(I()(Z)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(H1)({ai0:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([wa(fr(Cr)(a)(st()))(function(u){return function(o){return Lt(ct)(.8)(Gr(Or)(en(0)(40))(Fa(xn)(jr)(function(i){return gn(fn(St(Et()(z(z(ht)(kn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(Ct()())))({frequency:200+i*150,q:30})([u])})))}})])}}))})}}};var V1=function(){return d.value}(),SS=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(I()(Z)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(V1)({ai0:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return At(n)([uu(function(u){return Lt(ct)(1)([zn(Ga)(a)(st()),yo(Ze)(.1)([Lt(ct)(.6)([u])])])})])}}))})}}};var J1=function(){return d.value}(),j1=function(t){return function(r){return l(t)(En(r)(On)({p:[1,1,0],o:0,d:10}))}},Q1=function(t){return function(r){return l(t)(En(r)(On)({p:[1,1,0],o:0,d:8}))}},hl=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return yo(t)(n)([Lt(r)(a)([al(e)(u)(o)])])}}}}}}},TS=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(I()(I()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(J1)({txt:P(ie(`dgh d g h i =
  delay_ d [gain_ g [highpass_ h i]]

fade0 = pure
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 8.0 }

fade1 = pure
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
  ]`)),ai0:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return At(n)([wa(zn(Ga)(a)(st()))(function(u){return function(o){return uu(function(i){return Lt(ct)(1)([u,hl(Ze)(ct)(au)(.15)(.7)(1500)([uu(function(f){return Ue(ct)(1)(j1(C)())([hl(Ze)(ct)(au)(.4)(.5)(2500)([i,f])])})]),hl(Ze)(ct)(au)(.29)(.85)(2e3)([uu(function(f){return Lt(ct)(1)([hl(Ze)(ct)(au)(.6)(.6)(3500)([i,uu(function(p){return Ue(ct)(1)(Q1(C)())([hl(Ze)(ct)(au)(.75)(.6)(4e3)([f,p]),hl(Ze)(ct)(au)(.75)(.55)(3e3)([u])])})])])})])])})}})])}}))})}}};var K1=function(){return d.value}(),xS=function(t){return function(r){return function(e){return function(n){var a=function(u){return Pa(K)(C)(n)(dt(kt)(r(u))(ln))};return pr({reflectType:function(){return`<section>
  <p>
    In the <a @hwLink@ style="cursor:pointer;">hello world</a> section, we saw how to create and wire up two audio nodes: a <code>sinOsc</code>, or a sine-wave oscillator, is hooked up to a <code>gain</code> node. For some cases, feeding one audio node to another all the way up to a loudspeaker will be all you need. However, in most cases, you'll need to exploit three additional relationships:</p>
    <ul>
      <li><span style="font-weight:800px;">Many to one</span>, where many audio units pass through one.</li>
      <li><span style="font-weight:800px;">One to many</span>, where a single audio unit passes through many different ones.</li>
      <li><span style="font-weight:800px;">Feedback</span>, where an audio unit is an input to itself.</li>
    </ul>
    <p>This section will show how ocarina handles all three cases:</p>
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
  </section>`}})({reflectType:function(){return"@"}})()()(on()(Z)({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}}))(d.value)(K1)({hwLink:a(uf.value)})}}}};var Z1=function(){return d.value}(),FS=function(t){return function(r){return function(e){return function(n){var a=function(o){return Pa(K)(C)(n)(dt(kt)(r(o))(ln))},u=Ia(t)(e);return pr({reflectType:function(){return`<div>
  <h1>Array, fan, and fix</h1>

  <h3>The anatomy of a Ocarina graph</h3>

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

  <blockquote>If you don't have some sort of delay line in your processing chain, either via the Web-Audio-provided delay line or a custom delay node, Web Audio will raise a runtime error. Ocarina doesn't check for this, so make sure you test your audio to guarantee that it's feedback-explosion-free!</blockquote>

  <p>Nothing stops you from nesting <code>fix</code>-s to create a mega-feedback loop!</p>

  <blockquote>In the example below, I've added a couple fades to make sure the experience isn't too unpleasant. We'll talk more about fades in the events section \u{1F3B8}</blockquote>

  @code5@

  <h2>Next steps</h2>
  <p>In this section, saw how to combine together audio nodes with arrays, fan one audio node to many processing chains via <code>fan</code>, and how to create a fixed point, aka feedback, for a node via <code>fix</code>. In the next section, we'll ramp up on all of the yummy <a @next@ style="cursor:pointer;">audio nodes you can use</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(I()(I()(I()(I()(I()(I()(I()(on()(Z)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}}))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}}))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}}))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}}))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}}))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}}))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}}))(d.value)(Z1)({intro:P(xS(t)(r)(e)(n)),next:a(op.value),code0:P(gS(u)(r)(n)),code1:P(CS(u)(r)(n)),code2:P(hS(u)(r)(n)),code3:P(ES(u)(r)(n)),code4:P(SS(u)(r)(n)),code5:P(TS(u)(r)(n))})}}}};var OS=function(t){return function(r){return new F(Q("code")(t)(O()(L(O()(G(j)(r))))))}},ny=OS(T(A));var $S=function(t){return function(r){return new F(Q("pre")(t)(O()(L(O()(G(j)(r))))))}},ay=$S(T(A));var nB=function(){return d.value}(),wS=function(t){return function(r){return function(e){return function(n){var a=dt(kt)(r(ip.value))(ln),u=Ia(t)(e);return pr({reflectType:function(){return`<div>
  <h1>Hello world</h1>

  <h3>Wagging at 440Hz</h3>

  <p>Here's a "hello world" in Ocarina. In this and all the following sections, we'll start with a full example, and we'll pick it apart afterwards.</p>

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

  <p>Our sine wave oscillator is set to a frequency of <code>440Hz</code>. That means that your loudspeaker or headphones will vibrate back and forth in sinusoidal motion 440 times per second, which most folks perceive as the <a href="https://en.wikipedia.org/wiki/A440_(pitch_standard)">note A</a>. And we turn on the oscillator with <code>bangOn</code>, as the default is off for <i>all</i> sound generators in Ocarina. This is a design decision to help preserve the hearing of those that work frequently with audio.</p>

  <h2>Next steps</h2>
  <p>Now that we have our setup running, let's explore the anatomy of a Ocarina graph. Irrespective of the nodes comprising the graph, there are three basic concepts you need to be familiar with before you start diving into audio units: <a @next@ style="cursor:pointer;">array, fan, and fix</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(I()(on()(I()(Z)({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}}))(d.value)(nB)({code:P(ay([ny([ie(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])])),result:P(Mt(n)(u)(function(o){return l(Ca)(void 0)})(function(o){return function(i){return At(o)([Lt(ct)(.15)([af(sc)(440)(st())])])}})),next:Pa(K)(C)(n)(a)})}}}};var MS=wf;var PS=function(){return function(t){return t}},IS=function(){return function(t){return t}};var uy=function(){function t(){}return t.value=new t,t}();var RS={attr:function(t){return function(r){return b({key:"height",value:B(r)})}}};var oy=function(){function t(){}return t.value=new t,t}();var NS={attr:function(t){return function(r){return b({key:"width",value:B(r)})}}};var iy=function(t){return function(r){return new F(Q("canvas")(t)(O()(L(O()(G(j)(r))))))}};var cy=function(){function t(){}return t.value=new t,t}();var fy={attr:function(t){return function(r){return b({key:"@self@",value:ft(r)})}}};function Om(t){return function(){return t.getContext("2d")}}function kp(t){return function(r){return function(){t.fillStyle=r}}}function $m(t){return function(){t.beginPath()}}function wm(t){return function(){t.fill()}}function ly(t){return function(r){return function(){t.arc(r.x,r.y,r.radius,r.start,r.end,r.useCounterClockwise)}}}function Mm(t){return function(r){return function(){t.fillRect(r.x,r.y,r.width,r.height)}}}var kB=function(){return 2*ic}(),El=function(t){return{o:t.value0+.04,n:t.value1,t:Yo}};var gB=function(){return d.value}(),Sl=function(t){return function(r){return function(e){return function(n){return l(t)(go(r)(On)({p:[e,n],o:0,d:16}))}}}},CB=function(t){return function(r){return l(t)(En(r)(On)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},hB=function(t){return function(r){return l(t)(En(r)(On)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}};var Pm=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return function(i){return function(f){return function(p){return ep(t)(n)(a)([Ue(r)(u)(o)([Ad(e)(i)(f)(p)])])}}}}}}}}}},LS=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return function(i){return function(f){return function(p){return ep(t)(n)(a)([Ue(r)(u)(o)([yd(e)(i)(f)(p)])])}}}}}}}}}},EB=function(t){return function(r){return function(e){return function(n){return l(t)(dl(r)(On)({p:[e,n],o:0,d:16}))}}}},BS=400,_y=jr(BS),SB=function(){return Gt(Ba)(BS)+"px"}(),WS=600,py=jr(WS),TB=function(){return Gt(Ba)(WS)+"px"}(),xB={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},US=function(t){return function(r){return function(e){return pr({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(I()(Z)({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(d.value)(gB)({ex1:P(Wu()(oe({reflectSymbol:function(){return"canvas"}})()()(oe({reflectSymbol:function(){return"slider"}})()()(Lu({reflectSymbol:function(){return"startStop"}})()()()(oe({reflectSymbol:function(){return"loading"}})()()(oe({reflectSymbol:function(){return"start"}})()()(oe({reflectSymbol:function(){return"stop"}})()()(Nn)()())()())()())(Nn)()())()())()())(d.value)(function(n){return function(a){var u=X(K)(l(C)(void 0))(a.startStop.start),o=function(i){return function(f){return function(p){var _=m(g)(function(s){return new rt(s.acTime,s.value)})(yl(i)(a.slider));return[om(um(St(Et()(z(z(ht)(am)()()()({reflectSymbol:function(){return"fftSize"}}))(nm)()()()({reflectSymbol:function(){return"cb"}})))(Ct()())))({cb:function(s){return function(){return ne(new S(s))(p)(),ne(w.value)(p)}},fftSize:Js.value})(l(De)(wa(zn(Ga)(f)(X(K)(st())(m(g)(function(){var s=ia()(Re),v=qn(An)(Ra(0)(.96)(100)(1.04));return function(c){return s(El(v(c)))}}())(_))))(function(s){return function(v){return uu(function(c){return Lt(ct)(1)([s,ep(vd(St(Et()(z(z(ht)(Oh)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(nd)()()()({reflectSymbol:function(){return"delayTime"}})))(Ct()())))({maxDelayTime:2.5,delayTime:1})(m(g)(function(){var D=dl()(Re),M=qn(An)(Ra(0)(.5)(100)(2.45));return function(et){return D(El(M(et)))}}())(_))([Ue(ct)(.4)(m(g)(function(){var D=En()(Re),M=qn(An)(Ra(0)(.6)(100)(.9));return function(et){return D(El(M(et)))}}())(_))([s])]),Pm(Ze)(ct)(au)(.15)(T(A))(.7)(T(A))(1500)(Sl(C)()(1500)(3e3))([uu(function(D){return Ue(ct)(1)(CB(C)())([Pm(Ze)(ct)(au)(.4)(T(A))(.5)(T(A))(3e3)(Sl(C)()(3e3)(100))([c,D])])})]),Pm(Ze)(ct)(au)(.29)(m(g)(function(){var D=dl()(Re),M=qn(An)(Ra(0)(.1)(100)(.4));return function(et){return D(El(M(et)))}}())(_))(.85)(T(A))(2e3)(Sl(C)()(2e3)(5e3))([uu(function(D){return Lt(ct)(1)([Pm(Ze)(ct)(au)(.6)(m(g)(function(){var M=dl()(Re),et=qn(An)(Ra(0)(.8)(100)(.3));return function(Ft){return M(El(et(Ft)))}}())(_))(.6)(T(A))(3500)(Sl(C)()(3500)(100))([c,uu(function(M){return Ue(ct)(1)(hB(C)())([LS(Ze)(ct)(Dd)(.75)(m(g)(function(){var et=dl()(Re),Ft=qn(An)(Ra(0)(.9)(100)(.1));return function(Wt){return et(El(Ft(Wt)))}}())(_))(.6)(T(A))(4e3)(Sl(C)()(4e3)(200))([D,M]),LS(Ze)(ct)(Dd)(.75)(EB(C)()(.75)(.2))(.55)(T(A))(200)(Sl(C)()(200)(4e3))([s])])})])])})])])})}})))]}}};return zr([iy(X(K)(_n(_t)(A)(l(C))([tt(NS)(oy.value)(TB),tt(RS)(uy.value)(SB),tt(ag)(Jt.value)("width: 100%;"),tt(fy)(cy.value)(function(){var i=Ie(vt)(Zr)(function(f){return function(){var _=Om(f)();return kp(_)("black")(),Mm(_)({width:py,height:_y,x:0,y:0})(),void 0}});return function(f){return i(SD(f))}}())]))(m(g)(function(i){return tt(fy)(cy.value)(function(){var f=Ie(vt)(Zr)(function(p){return function(){var s=Om(p)();return kp(s)("black")(),Mm(s)({width:py,height:_y,x:0,y:0})(),kp(s)("rgba(255,255,255,0.2)")(),Zu(i)(function(v){return function(){return $m(s)(),ly(s)({end:kB,radius:v.value1*40,start:0,x:v.value0.x*py,y:v.value0.y*_y,useCounterClockwise:!1})(),wm(s)()}})()}});return function(p){return f(SD(p))}}())})(a.canvas)))([]),ho(_n(_t)(A)(l(C))([tt(ni)(Co.value)("range"),tt(yc)(Ri.value)("0"),tt(bc)(Ii.value)("100"),tt(kc)(Li.value)("1"),tt(gc)(Bi.value)("50"),tt(ng)(Jt.value)("width: 100%;"),tt(Ac)(Ni.value)(te(function(){var i=Ie(vt)(Zr)(mf(Zn)(Xc)(n.slider)),f=jn(ma)(Hf);return function(p){return i(f(Cl(p)))}}()))]))([]),Cn(Yt(_t)(A)([l(C)(tt(Pf)(Jt.value)("width:100%; padding:1.0rem;")),_n(_t)(A)(m(g)(function(){var i=tt(be)(se.value);return function(f){return i(te(E(f)))}}()))([Y(g)(a.startStop.loading)(l(vt)(void 0)),Gr(g)(a.startStop.stop)(function(i){return dt(kt)(i)(dt(kt)(t(l(vt)(void 0)))(n.startStop.start(void 0)))}),Gr(g)(yt(mn)(Y(g)(u)(nt(at)))(X(K)(l(C)(l(vt)(void 0)))(m(g)(function(i){return i.value0})(e))))(function(i){return function(){i(),n.startStop.loading(void 0)();var p=Br(w.value)(),_=ko(mt(Pe)(oa(Me))(function(s){return mt(Pe)(Vu(Me)(s))(function(v){return mt(Pe)(m(Oi)(IS())(y0(wn)(MS)(xt(s))(PS()(xB))))(function(c){return mt(Pe)(ye(Me)(S_(0)(5e4)))(function(D){var M=pc(hD(sa($_(c.pluck0))(Lf(jv(Qv()(c))))))({newSeed:cc(D),size:4});return ye(Me)(function(){var Ft=Rn(Oo)(vt)(function(lt){return function(){var Fr=Jo(),Pt=Jo();return{x:Fr,y:Pt}}})(en(0)(127))(),Wt=At(s)(o(s)(M)(p))(),Xr=Be(vc)(function(lt){return function(){var Fr=Jr(p)();return pn(vt)(Zr)(Fr)(function(Pt){return function(){var Vn=_p(Pt)(),qe=m(N)(function(){var Ea=Ql(Ft),ca=m(Or)(function(He){return function(Gu){return Gu/255}(He)});return function(He){return Ea(ca(He))}}())(bm(dm)(Vn))();return n.canvas(qe)(),void 0}})()}})(),ve=dt(kt)(dt(kt)(dt(kt)(Wt)(v))(hn(ce)(s)))(Xr);return n.startStop.stop(ve)(),ve})})})})}))();return t(function(){return n.startStop.start(void 0)(),ti(wi(_))()})(),void 0}})])]))([un(Yt(_t)(A)([m(g)(E("Turn off"))(a.startStop.stop),m(g)(E("Turn on"))(u),m(g)(E("Loading..."))(a.startStop.loading)]))])])}}))})}}};var OB=function(){return d.value}(),qS=function(t){return function(r){return function(e){return function(n){var a=Ia(t)(e);return It({reflectType:function(){return`<div>
  <h1>Ocarina</h1>

  <h3>A web-audio framework written in PureScript</h3>

  <p>Hi! You've found <a href="https://github.com/mikesol/purescript-ocarina">Ocarina</a>.</p>

  <p>Ocarina is a web-audio framework designed for interactive media and games. Events like mouse clicks, MIDI notes and tweening frames are streamed to an audio rendering engine and, in response to these events, sound happens.</p>

  <h2>Why?</h2>

  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API">Web Audio API</a> is an amazing piece of technology. It is clear, concise, straightforward and ergonomic. So why build a framework on top of it?</p>

  <p>As audio projects become more and more ambitious, a need emerges for powerful abstractions to handle browser events and scheduling. Ocarina tackles this problem through a small set of <a href="https://en.wikipedia.org/wiki/Functional_reactive_programming">FRP</a>-based abstractions. In doing so, it aims to be concise, expressive, and as fast as manually-optimized hand-written JavaScript.</p>

  <h2>How does it sound?</h2>

  <p>Here's a small example in Ocarina that, when you turn it on, emits a single sound and then uses feedback loops to create long tail. You can use the slider to change the properties of the tail in real time.</p>

  ~ex~

  <p>By the end of this documentation, you'll know all of the concepts you need to create interactive audio like the example above.</p>

  <p>If you'd like to use this documentation as a springboard for your own work, it can be found <a href="https://github.com/mikesol/purescript-ocarina/tree/main/examples/docs">here</a>.</p>

  <p>And now, without further ado, let's write a small <a ~next~ style="cursor:pointer;">hello world \xE0 la ocarina</a>!</p>
</div>`}})()()(I()(on()(Z)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}}))(OB)({next:Pa(K)(C)(n)(dt(kt)(r(uf.value))(ln)),ex:P(US(a)(r)(n))})}}}};var wB=function(){return d.value}(),HS=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<div>
  <h1>Merge and split</h1>

  <h3>Inputs and outputs abound!</h3>
  <p>
    Web audio allows you to merge and split arbitrary audio. This is essential when you're working with complex audio setups like 5.1 surround sound or novel headphones used in some gaming setups. Ocarina allows you to both split and merge arbitrary signals using Web Audio's native merger and splitter nodes.
  </p>

  <h2>Merging</h2>

  <p>Merging audio in ocarina looks like any node that takes multiple inputs, but instead of accepting something of type <code>AudioInput</code>, it accepts a <i>vector of audio inputs</i>.</p>

  <h2>Splitting</h2>

  <p>Splitting is the inverse operation of merging: it takes a single audio node and splits it into its separate channels. In doing so, it resembles <code>fan</code>, but instead of fanning the audio, it splits it into mono-channel audio.</p>

  <h2>Next steps</h2>
  <p>In this section, saw how to merge and split audio. In the next section, we'll look at how to work with <a ~next~ style="cursor:pointer;">custom audio worklets</a>.</p>
</div>`}})()()(on()(Z)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(wB)({next:l(C)(tt(be)(se.value)(te(E(dt(kt)(r(fm.value))(ln)))))})}}}};var PB=function(){return d.value}(),zS=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(I()(I()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}}))(PB)({txt:P(ie(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1000
            $ pure
            $ playbackRate
            $ AudioEnvelope
                { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                , o: 1.5
                , d: 30.0
                }
          delay 3000 (pure (playbackRate (AudioCancel { o: 3.5 })))
      ]
  ]`)),cancel:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([Lt(ct)(1)([fr(Cr)(a)(Yt(_t)(A)([st(),Nu(1e3)(l(C)(ia()(On)({p:So(Eo)(Y(Or)(en(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),Nu(3e3)(l(C)(ia()(Dh)({o:3.5})))]))])])}}))})}}};var RB=function(){return d.value}(),VS=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(I()(I()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}}))(RB)({txt:P(ie(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          OneOf.do
            bangOn
            delay 1000
              $ pure
              $ playbackRate
              $ AudioEnvelope
                  { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                  , o: 1.5
                  , d: 30.0
                  }
          )
      ]
  ]`)),envelope:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([Lt(ct)(1)([fr(Cr)(a)(Yt(_t)(A)([st(),Nu(1e3)(l(C)(ia()(On)({p:So(Eo)(Y(Or)(en(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}}))})}}};var LB=function(){return d.value}(),GS=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
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
      [ loopBuf buf OneOf.do
          bangOn
          delay 1000 OneOf.do
            pure
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 1.0, t: _step }
            pure
              $ playbackRate
              $ AudioNumeric { n: 1.3, o: 2.0, t: _linear }
          delay 2500 OneOf.do
            pure
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 2.5, t: _step }
            pure
              $ playbackRate
              $ AudioNumeric { n: 0.7, o: 3.5, t: _exponential }
      ]
  ]</code></pre>

  @numericEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(I()(Z)({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}}))(d.value)(LB)({numericEx:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([Lt(ct)(1)([fr(Cr)(a)(ai(K)(st())(function(){return ai(K)(Nu(1e3)(ai(K)(l(C)(ia()(Re)({n:1,o:1,t:YD})))(function(){return l(C)(ia()(Re)({n:1.3,o:2,t:Yo}))})))(function(){return Nu(2500)(ai(K)(l(C)(ia()(Re)({n:1,o:2.5,t:YD})))(function(){return l(C)(ia()(Re)({n:.7,o:3.5,t:vh}))}))})}))])])}}))})}}};var WB=function(){return d.value}(),JS=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
  <h2>AudioSudden</h2>
  <p>The simplest change you can make is scheduling a value to change <i>now</i>. This is done with <code>AudioSudden</code>, which is a wrapper around the setter for an audio parameter's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/value"><code>value</code></a> field in the Web Audio API.</p>

  <p>In the example below, we change a value after it has run for 1.5 seconds.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1500
            $ pure
            $ playbackRate
            $ AudioSudden { n: 1.4 }
      ]
  ]</code></pre>

  @suddenEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(I()(Z)({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}}))(d.value)(WB)({suddenEx:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([Lt(ct)(1)([fr(Cr)(a)(Yt(_t)(A)([st(),Nu(1500)(l(C)(ia()(sh)({n:1.4})))]))])])}}))})}}};var qB=function(){return d.value}(),jS=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2>Audio Units</h2>
  <p>In my humble opinion, the summit of Web Audio programming is when audio units control the audio parameters of other audio units. This allows for a form of radical experimentation that is difficult in many other frameworks. <a href="https://www.w3.org/TR/webaudio/#ModularRouting">Nearly any audio parameter</a> can be automated this way.</p>

  <p>To control an audio parameter with an audio unit, use the <code>AudioUnit</code> constructor. You can also use a <code>Node D1 l p</code>. If your node is for an arbitrary number of channels, make sure to coerce it to mono using the <code>c1</code> function, as in the example below.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ loopBuf buf OneOf.do
      bangOn
      pure
        $ playbackRate
        $ c1
            ( gain_ 1.0
                [ constant 1.0 bangOn
                , gain_ 0.2 (lowpass_ 100.0 (squareOsc 50.0 bangOn))
                ]
            )
      )
  ]
</code></pre>

  ~unitEx~
  </section>
`}})()()(I()(Z)({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}}))(qB)({unitEx:P(Mt(e)(t)(function(n){return xt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return At(n)([fr(Cr)(a)(Yt(_t)(A)([st(),l(C)(ia()(lh(hi)(hi))(fh(Lt(ct)(1)([cm(Zs)(1)(st()),Lt(ct)(.2)([ul(rm)(100)([tp(Qf)(50)(st())])])]))))]))])}}))})}}};var zB=function(){return d.value}(),QS=function(t){return function(r){return function(e){return function(n){var a=dt(kt)(r(fp.value))(ln),u=Ia(t)(e);return It({reflectType:function(){return`<div>
  <h1>Parameters</h1>

  <h3>Controlling our units</h3>
  <p>
    In the previous section, we saw how we can use browser events to control audio units. The Web Audio API provides a rich set of tools to control both the audio-rate and control-rate parameters of audio units. This section goes over how ocarina exposes those parameters.
  </p>

  ~sudden~
  ~numeric~
  ~envelope~
  ~cancel~
  ~unit~

  <h2>Next steps</h2>
  <p>In this section, we saw how to specify parameters for audio units, including using audio-rate audio units as parameters. In the next section, we'll look at how to make events <a ~next~ style="cursor:pointer;">stateful</a>.</p>
</div>`}})()()(I()(I()(on()(I()(I()(I()(Z)({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}}))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}}))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}}))(zB)({sudden:P(JS(u)(r)(n)),numeric:P(GS(u)(r)(n)),envelope:P(VS(u)(r)(n)),cancel:P(zS(u)(r)(n)),unit:P(jS(u)(r)(n)),next:Pa(K)(C)(n)(a)})}}}};var GB=function(){return d.value}(),XS=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<div>
  <h1>Tumult</h1>

  <h2>The unknown unknowns of web audio</h2>
  <p>
    As we saw in the last section on subgraphs, sometimes, you simply don't know how your audio will evolve. For example, if we are building a live coding environment and create a space where someone can basically do <i>anything</i>, working with anonymous audio nodes will not cut it. For example, imagine that they have two relatively unconnected parts of the audio graph and want to start creating cross-connections. With subgraphs, we can't pick apart the graph and say "take node X and connect it to node Y" as nodes don't have names.
  </p>

  <p>
    Tumult solves this problem. It is an entirely separate rendering engine from the one we've seen before that works via <i>diffing</i> two audio graphs and adjusting the current graph based on the diff. The biggest difference is that tumult <i>requires</i> you to give a name to each audio unit. Otherwise, the engine won't know what node corresponds to what name.
  </p>

  <p>
    For you veteran coders and audio-ers, you may bristle at the word <i>diff</i>. After all, diffing is expensive, and we are working in resource constrained environments where timing is of the essence. Fear not! Tumult uses PureScript <code>Set</code>-s to do blazingly fast diffs. That said, tumult is the slowest part of ocarina (meaning it is just blazingly fast instead of obscenely and indecently fast), so only use it where you can afford a performmance hit.
  </p>

  <h2>Hello tumult</h2>

  <h2>Stability in tumult</h2>

  <p>While tumult will run a diff every time its event is triggered, if you don't tear down a node, its nodes will merrily putter along with whatever events you send them. This means that you can trigger tumult events <i>only</i> when you need to change the shape of the graph.</p>

  <h2>Feeling lucky</h2>

  <p>So far, we have only seen the <code>DiffMe</code> instruction sent to tumult. There is another instruction you can send called <code>FeelingLucky</code>. <code>FeelingLucky</code> will attempt to perform its instruction and be a no-op if it is incoherent with respect to the graph. Careful with this, though! <code>DiffMe</code> guarantees that whatever you send to tumult will be what's rendered from your loudspeaker. Too many <code>FeelingLucky</code>-s, on the other hand, can lead to bugs where you're not quite sure anymore <i>what</i> is playing. That said, <code>FeelingLucky</code> is grotesquely fast, especially for larger graphs. So if it makes sense to use it, use it!</p>

  <h2>Next steps</h2>
  <p>In this section, we learned how to use tumult to create truly dynamic audio graphs that allow you to fine-tune the speed-versus-dynamism tradeoff in various ways.</p>
  <p>In the next section, we'll look at how to create audio graphs via an <a ~next~ style="cursor:pointer;">imperative API that more closely resembles Web Audio while providing additional type-safety benefits</a>.</p>
</div>`}})()()(on()(Z)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(GB)({next:l(C)(tt(be)(se.value)(te(E(dt(kt)(r(lm.value))(ln)))))})}}}};var jB=function(){return d.value}(),KS=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<div>
  <h1>Audio Worklets</h1>

  <h2>Supercharging our audio experiences</h2>

  <p>The Web Audio lets you do basically anything, but when anything isn't enough, there are custom Audio Worklets. </p>

  <p>Custom audio worklets come in a few shapes an sizes:</p>

  <ul>
    <li>You can <a href="https://developer.chrome.com/blog/audio-worklet/">write your own in JavaScript</a>. While this is ok for small PoCs, it will get really slow really quick.</li>
    <li>You can compile your favorite C/C++/Rust audio processors to <a href="https://developer.chrome.com/blog/audio-worklet-design-pattern/#using-audio-worklet-with-webassembly">web assembly</a>. The helpful folks at Google have created many examples that do exactly this, and you can pilfer them for fun and profit.</li>
    <li>My go-to solution is Faust. <a href="https://faust.grame.fr/">Faust</a> is the greatest audio project on Earth, and let's you build very performant audio processors with an expressive and concise syntax. Faust's <a href="https://webaudioconf.com/posts/2017_EA_60/">Web Audio integration</a> is a one-liner from the command line.</li>
  </ul>

  <p>In this example, we'll use Faust to create a three custom audio units and wire them together using Ocarina. The units are:</p>

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

  We can compile each example using the following commands. Each command creates a self-contained JavaScript file, so all we need to do is link to it in ocarina-land.

  <pre><code>placeholder</code></pre>

  <h2>Ocarina</h2>

  <p>Ocarina provides a type-safe interface for declaring the API of Audio Worklets. While it's your job to make sure the API is in fact the actual API of the worklet, assuming this is correct, ocarina will enforce it for you. Let's see how in the following example. Here's the code:</p>

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
  <p>In this section, we created three audio worklet nodes using Faust and used them in the Web Audio API via ocarina. There is active work going on to bundle all this into a single toolchain so that Faust can be written directly in PureScript and automatically read as an Audio Worklet by ocarina. Until that happens, though, this is a great solution: just make sure to get the parameter names right across the language barrier! No amonut of type-safety can save you there \u{1F605}</p>
  <p>In the next section, we'll look at how to create <a ~next~ style="cursor:pointer;">mutable state in a ocarina graph</a>.</p>
</div>`}})()()(on()(Z)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(jB)({next:l(C)(tt(be)(se.value)(te(E(dt(kt)(r(of.value))(ln)))))})}}}};var XB=function(){return d.value}(),YS=function(t){return function(r){return function(e){return function(n){return pr({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out ocarina! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-ocarina">Ocarina GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Now go out there and play some ocarina!</p>
</div>`}})({reflectType:function(){return"~"}})()()(Z)(d.value)(XB)({})}}}};var YB=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\\))
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable)
import Bolson.Core (envy)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (create, fold, makeEvent, subscribe, delay)

import FRP.Event.VBus (V, vbus)
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import Ocarina.Control (gain_, playBuf)
import Ocarina.Core (Channel(..), dyn, bangOn)
import Ocarina.Interpret (bracketCtx, decodeAudioDataFromUri)
import Ocarina.Run (run2_)
import Ocarina.WebAPI (BrowserAudioBuffer)

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
    -> Domable Effect lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ pure $ envy $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        startE = pure unit <|> event.startStop.start
        sl = sampleBy (/\\) random
          $ fold (\\_ b -> b + 1) event.slider 0
        music = run2_
          [ gain_ 1.0
              [ dyn $ map
                  ( \\i ->
                      OneOf.do
                        pure $ sound $ playBuf
                          { buffer: buffer, playbackRate: 0.7 + (fst i) * 2.0 }
                          bangOn
                        delay 5000 $ pure $ silence
                  )
                  sl
              ]
          ]
      D.div_
        [ D.div_
            [ text_ "Slide me!"
            , D.input
                ( O.oneOfMap pure O.do
                    D.Xtype := "range"
                    D.Min := "0"
                    D.Max := "100"
                    D.Step := "1"
                    D.Value := "50"
                    D.OnInput := cb (const (push.slider unit))
                )
                []
            ]
        , D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                startE $> (music >>= push.startStop.stop)
                event.startStop.stop <#>
                  (_ *> push.startStop.start unit)
            )
            [ text OneOf.do
                startE $> "Turn on"
                event.startStop.stop $> "Turn off"
            ]
        ]
`,ZB=_u(function(t){return xa(function(r){return Be(t)(function(e){return function(){var a=Jo();return r(e(a))()}})})}),t3=function(){return d.value}(),r3="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",ZS=function(t){return function(r){return function(e){return pr({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels lock payload))</code>. Streaming audio is a data type with two constructors: <code>sound</code> to create a subgraph and <code>silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: ocarina automatically frees up resources when you send the <code>silence</code> event. Note that, once you turn a subraph off with <code>silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(I()(I()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(d.value)(t3)({txt:P(ie(YB)),ex1:P(Wu()(oe({reflectSymbol:function(){return"slider"}})()()(Lu({reflectSymbol:function(){return"startStop"}})()()()(oe({reflectSymbol:function(){return"loading"}})()()(oe({reflectSymbol:function(){return"start"}})()()(oe({reflectSymbol:function(){return"stop"}})()()(Nn)()())()())()())(Nn)()())()())(d.value)(function(n){return function(a){var u=X(K)(l(C)(void 0))(a.startStop.start),o=Pi(g)(rt.create)(ZB)(Ru(Tr)(function(f){return function(p){return f+1|0}})(0)(a.slider)),i=function(f){return[Lt(ct)(1)([d_(m(g)(function(p){return Yt(_t)(A)([l(C)(ih(zn(Y_(St(Et()(z(z(ht)(Ch)()()()({reflectSymbol:function(){return"playbackRate"}}))(Q_)()()()({reflectSymbol:function(){return"buffer"}})))(Ct()())))({buffer:f,playbackRate:.7+qa(p)*2})(st()))),Nu(5e3)(l(C)(ch))])})(o))])]};return zr([zr([ie("Slide me!"),ho(_n(_t)(A)(l(C))([tt(ni)(Co.value)("range"),tt(yc)(Ri.value)("0"),tt(bc)(Ii.value)("100"),tt(kc)(Li.value)("1"),tt(gc)(Bi.value)("50"),tt(Ac)(Ni.value)(te(E(n.slider(void 0))))]))([])]),Cn(_n(_t)(A)(m(g)(function(){var f=tt(be)(se.value);return function(p){return f(te(E(p)))}}()))([Y(g)(a.startStop.loading)(l(vt)(void 0)),Gr(g)(a.startStop.stop)(function(f){return dt(kt)(f)(dt(kt)(t(l(vt)(void 0)))(n.startStop.start(void 0)))}),Gr(g)(yt(mn)(Y(g)(u)(nt(at)))(X(K)(l(C)(l(vt)(void 0)))(m(g)(function(f){return f.value0})(e))))(function(f){return function(){f(),n.startStop.loading(void 0)();var _=ko(mt(Pe)(oa(Me))(function(s){return mt(Pe)(Vu(Me)(s))(function(v){return mt(Pe)(xt(s)(r3))(function(c){return ye(Me)(function(){var M=Dm(i(c))(),et=dt(kt)(dt(kt)(M)(v))(hn(ce)(s));return n.startStop.stop(et)(),et})})})}))();return t(function(){return n.startStop.start(void 0)(),ti(wi(_))()})(),void 0}})]))([un(Yt(_t)(A)([m(g)(E("Turn off"))(a.startStop.stop),m(g)(E("Turn on"))(u)]))])])}}))})}}};var n3=function(){return d.value}(),tT=function(t){return function(r){return function(e){return function(n){var a=Ia(t)(e);return It({reflectType:function(){return`<div>
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
  <p>Thus ends the first version of the ocarina documentation. Applause is always welcome ~appl~! Alas, some features remain undocumented, like audio worklets and an imperative API. At some point I hope to document all of these, but hopefully this should be enough to get anyone interested up and running. If you need to use any of those features before I document them, ping me on the <a href="https://purescript.org/chat">PureScript Discord</a>. Otherwise, happy music making with Ocarina!</p>
</div>`}})()()(I()(I()(Z)({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}}))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}}))(n3)({appl:P(vm("\u{1F44F}")(n)(a)(function(u){return xt(u)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(u){return function(o){return At(u)([Lt(ct)(1)([fr(Cr)(o)(st())])])}})),suby:P(ZS(a)(r)(n))})}}}};var BBt=function(t){return t},WBt={Coercible0:function(){}},u3=function(){var t=function(r){var e=function(n){if(n instanceof up)return zr(l(De)(Ke(qS(r.setCancellation)(r.setPage))));if(n instanceof uf)return zr(l(De)(Ke(wS(r.setCancellation)(r.setPage))));if(n instanceof ip)return zr(l(De)(Ke(FS(r.setCancellation)(r.setPage))));if(n instanceof op)return zr(l(De)(Ke(TE(r.setCancellation)(r.setPage))));if(n instanceof fm)return zr(l(De)(Ke(KS(r.setCancellation)(r.setPage))));if(n instanceof of)return zr(l(De)(Ke(kS(r.setCancellation)(r.setPage))));if(n instanceof cp)return zr(l(De)(Ke(QS(r.setCancellation)(r.setPage))));if(n instanceof fp)return zr(l(De)(Ke(fS(r.setCancellation)(r.setPage))));if(n instanceof lm)return zr(l(De)(Ke(YS(r.setCancellation)(r.setPage))));if(n instanceof w0)return zr(l(De)(Ke(HS(r.setCancellation)(r.setPage))));if(n instanceof lp)return zr(l(De)(Ke(tT(r.setCancellation)(r.setPage))));if(n instanceof M0)return zr(l(De)(Ke(XS(r.setCancellation)(r.setPage))));throw new Error("Failed pattern match at Ocarina.Example.Docs (line 148, column 5 - line 148, column 76): "+[n.constructor.name])};return e(r.page)};return cg(ig(new _l(up.value)))(function(r){var e=Ru(Tr)(yr(function(n){if(n instanceof _l)return function(a){return{prevPage:new S(a.curPage),curPage:n.value0,cancel:a.cancel,pageChange:!0}};if(n instanceof hd)return function(a){return{cancel:n.value0,pageChange:!1,curPage:a.curPage,prevPage:a.prevPage}};throw new Error("Failed pattern match at Ocarina.Example.Docs (line 59, column 13 - line 61, column 81): "+[n.constructor.name])}))({prevPage:w.value,curPage:up.value,cancel:l(vt)(void 0),pageChange:!0})(r.value1);return zr([zr(m(Or)(function(n){return aD([nD(X(K)(_n(_t)(A)(l(C))([tt(be)(se.value)(te(E(r.value0(new _l(n.value0))))),tt(ug)(Jt.value)("cursor:pointer;")]))(m(g)(function(a){return tt(be)(se.value)(te(E(function(){return a.cancel(),r.value0(new _l(n.value0))()})))})(Tf(ru)(function(){var a=Iu(bu);return function(u){return a(function(o){return o.pageChange}(u))}}())(e))))([ie(n.value1.value0)]),Rf(l(C)(tt(cs)(Jt.value)(function(){return n.value1.value1?"":"display:none;"}())))([ie(" | ")])])})([new rt(up.value,new rt("Home",!0)),new rt(uf.value,new rt("Hello world",!0)),new rt(ip.value,new rt("Array, fan, and fix",!0)),new rt(op.value,new rt("Audio units",!0)),new rt(of.value,new rt("Events",!0)),new rt(cp.value,new rt("Parameters",!0)),new rt(fp.value,new rt("State",!0)),new rt(lp.value,new rt("Subgraphs",!1))])),tg(function(n){return t({page:n.curPage,setPage:function(a){return r.value0(_l.create(a))},setCancellation:function(a){return r.value0(hd.create(a))}})})(Tf(ru)(function(n){return n.pageChange})(e))])})}(),UBt=function(t){return{page:t,setPage:vr(xp(Vi(zi))),setCancellation:vr(xp(Vi(zi)))}},qBt=sC(u3);export{BBt as TopLevelSg,qBt as main,WBt as newtypeTopLevelSg_,UBt as p2tl,u3 as scene};
