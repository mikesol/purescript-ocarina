var vy=function(t){return function(r){for(var e=r.length,n=new Array(e),a=0;a<e;a++)n[a]=t(r[a]);return n}};var si={compose:function(t){return function(r){return function(e){return t(r(e))}}}},Hu=function(t){return t.compose};var rt=function(t){return t.identity},et={identity:function(t){return t},Semigroupoid0:function(){return si}};var oe=!0;var Mt=function(t){return function(r){return function(e){return t(e)(r)}}},T=function(t){return function(r){return t}};var ji=function(t){return function(r){return r(t)}},Df=function(t){return function(r){return t(r)}};var d=function(){function t(){}return t.value=new t,t}();var _=function(t){return t.map},Vr=function(t){return function(r){return function(e){return _(t)(e)(r)}}},cr=function(t){return _(t)(T(void 0))},K=function(t){return function(r){return function(e){return _(t)(T(e))(r)}}},df=function(t){return function(r){return _(t)(T(r))}};var Wn={map:Hu(si)},$r={map:vy},Wm=function(t){return function(r){return function(e){return _(t)(function(n){return n(e)})(r)}}};var Dy=function(t){return function(r){return t+r}},dy=function(t){return function(r){return t.length===0?r:r.length===0?t:t.concat(r)}};var ge=function(t){return t.reflectSymbol};var $c=function(t){var r=function(e){var n;function a(u){e=u}for(;;)n=a(e);return n};return r(t)};var Pl=function(t){return function(r){return{}.hasOwnProperty.call(r,t)}},Za=function(t){return function(r){return r[t]}},fo=function(t){return function(r){return function(e){var n={};for(var a in e)({}).hasOwnProperty.call(e,a)&&(n[a]=e[a]);return n[t]=r,n}}};var by={append:function(t){return function(r){return void 0}}},qm={append:Dy};var un={append:dy};var bt=function(t){return t.append},Zm=function(t){return{append:function(r){return function(e){return function(n){return bt(t)(r(n))(e(n))}}}}};var R=function(t){return t.alt};var yy=function(t){return function(r){for(var e=t.length,n=r.length,a=new Array(e*n),u=0,i=0;i<e;i++)for(var o=t[i],p=0;p<n;p++)a[u++]=o(r[p]);return a}};var Il={apply:yy,Functor0:function(){return $r}},Ut=function(t){return t.apply};var Q=function(t){return function(r){return function(e){return Ut(t)(_(t.Functor0())(T(rt(et)))(r))(e)}}},qn=function(t){return function(r){return function(e){return function(n){return Ut(t)(_(t.Functor0())(r)(e))(n)}}}};var l=function(t){return t.pure};var Zn=function(t){return function(r){return function(e){if(r)return e;if(!r)return l(t)(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[r.constructor.name,e.constructor.name])}}},Rl=function(t){return function(r){return function(e){return Ut(t.Apply0())(l(t)(r))(e)}}};var de={pure:function(t){return[t]},Apply0:function(){return Il}};var Ay=function(t){return function(r){for(var e=[],n=0,a=t.length;n<a;n++)Array.prototype.push.apply(e,r(t[n]));return e}};var Xr=function(t){return t.discard};var mi={bind:Ay,Apply0:function(){return Il}},P=function(t){return t.bind},zn=function(t){return Mt(P(t))};var bf=function(t){return function(r){return function(e){return function(n){return P(t)(r(n))(e)}}}};var Qr={discard:function(t){return P(t)}};var eu=function(t){return function(r){return P(t)(r)(rt(et))}};var yn=function(t){return function(r){for(var e=t>r?-1:1,n=new Array(e*(r-t)+1),a=t,u=0;a!==r;)n[u++]=a,a+=e;return n[u]=a,n}},sS=function(t){return function(r){if(t<1)return[];var e=new Array(t);return e.fill(r)}},mS=function(t){return function(r){for(var e=[],n=0,a=0;a<t;a++)e[n++]=r;return e}},gp=typeof Array.prototype.fill=="function"?sS:mS,vS=function(){function t(a,u){this.head=a,this.tail=u}var r={};function e(a){return function(u){return new t(a,u)}}function n(a){for(var u=[],i=0,o=a;o!==r;)u[i++]=o.head,o=o.tail;return u}return function(a){return function(u){return n(a(e)(r)(u))}}}(),xa=function(t){return t.length};var ky=function(t){return function(r){return function(e){return function(n){return n<0||n>=e.length?r:t(e[n])}}}};var gy=function(t){return function(r){return function(e){return function(n){for(var a=0,u=n.length;a<u;a++)if(e(n[a]))return t(a);return r}}}};var Cy=function(t){return function(r){return function(e){return function(n){if(e<0||e>=n.length)return r;var a=n.slice();return a.splice(e,1),t(a)}}}};var DS=function(){function t(r,e,n,a,u,i){var o,p,s,f,m,v,c;for(o=u+(i-u>>1),o-u>1&&t(r,e,a,n,u,o),i-o>1&&t(r,e,a,n,o,i),p=u,s=o,f=u;p<o&&s<i;)m=a[p],v=a[s],c=e(r(m)(v)),c>0?(n[f++]=v,++s):(n[f++]=m,++p);for(;p<o;)n[f++]=a[p++];for(;s<i;)n[f++]=a[s++]}return function(r){return function(e){return function(n){var a;return n.length<2?n:(a=n.slice(0),t(r,e,a,n.slice(0),0,n.length),a)}}}}();var yf=function(t){return function(r){return function(e){for(var n=r.length<e.length?r.length:e.length,a=new Array(n),u=0;u<n;u++)a[u]=t(r[u])(e[u]);return a}}};var hy=function(t){return function(r){return t[r]}};var bS=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var Ey={defer:function(t){return function(r){return t(void 0)(r)}}},Af=function(t){return t.defer},Gm=function(t){return function(r){var e=bS("go","Control.Lazy",function(){return Af(t)(function(a){return r(e(25))})}),n=e(25);return n}};var lo=function(t){return function(r){return function(e){return P(t.Bind1())(r)(function(n){return P(t.Bind1())(e)(function(a){return l(t.Applicative0())(n(a))})})}}};var yS=String.fromCharCode(65535),AS=String.fromCharCode(0),kS=Number.POSITIVE_INFINITY,gS=Number.NEGATIVE_INFINITY;var Ty=function(t){return function(r){return function(e){return function(n){return function(a){return n<a?t:n===a?r:e}}}}};var Sy=Ty,xy=Ty;var Fy=function(t){return function(r){return t===r}};var Oy=Fy,$y=Fy;var Nl={eq:$y},Xi={eq:Oy};var nr=function(t){return t.eq};var ar=function(){function t(){}return t.value=new t,t}(),Ar=function(){function t(){}return t.value=new t,t}(),kr=function(){function t(){}return t.value=new t,t}();var wy=function(t){return function(r){return t-r|0}},My=function(t){return function(r){return t-r}};var Py=function(t){return function(r){return t+r|0}},Iy=function(t){return function(r){return t*r|0}},Ry=function(t){return function(r){return t+r}},Ny=function(t){return function(r){return t*r}};var da=function(t){return t.zero};var Fa={add:Ry,zero:0,mul:Ny,one:1},Uu={add:Py,zero:0,mul:Iy,one:1};var ba=function(t){return t.one};var In=function(t){return t.mul};var Br=function(t){return t.add};var Eu=function(t){return t.sub};var wc={sub:My,Semiring0:function(){return Fa}},Jm={sub:wy,Semiring0:function(){return Uu}};var Ll=function(t){return function(r){return Eu(t)(da(t.Semiring0()))(r)}};var za=function(){return{compare:xy(ar.value)(kr.value)(Ar.value),Eq0:function(){return Nl}}}(),Ye=function(){return{compare:Sy(ar.value)(kr.value)(Ar.value),Eq0:function(){return Xi}}}();var ur=function(t){return t.compare};var By=function(t){return function(r){return function(e){var n=ur(t)(r)(e);return!(n instanceof ar)}}};var Wu=function(t){return function(r){return function(e){var n=ur(t)(r)(e);if(n instanceof ar)return e;if(n instanceof kr||n instanceof Ar)return r;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[n.constructor.name])}}};var Xm=function(t){return function(r){return function(e){var n=By(t)(e)(da(r.Semiring0()));return n?e:Ll(r)(e)}}};var ua=function(t){return t.top};var Pc={top:2147483647,bottom:-2147483648,Ord0:function(){return Ye}};var oa=function(t){return t.bottom};var Uy=function(t){return t.toString()},Wy=function(t){var r=t.toString();return isNaN(r+".0")?r:r+".0"};var hp={show:Wy},au={show:Uy};var jt=function(t){return t.show};var W=function(){function t(){}return t.value=new t,t}(),F=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Gt=function(t){return function(r){return function(e){if(e instanceof W)return t;if(e instanceof F)return r(e.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}};var Re={map:function(t){return function(r){return r instanceof F?new F(t(r.value0)):W.value}}};var ya=function(t){return Gt(t)(rt(et))},ca=function(){return function(t){if(t instanceof F)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var Ki={apply:function(t){return function(r){if(t instanceof F)return _(Re)(t.value0)(r);if(t instanceof W)return W.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,r.constructor.name])}},Functor0:function(){return Re}},Oa={bind:function(t){return function(r){if(t instanceof F)return r(t.value0);if(t instanceof W)return W.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,r.constructor.name])}},Apply0:function(){return Ki}};var Uo=function(){return{pure:F.create,Apply0:function(){return Ki}}}();var Kt=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Yt=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Rc={map:function(t){return function(r){if(r instanceof Kt)return new Kt(r.value0);if(r instanceof Yt)return new Yt(t(r.value0));throw new Error("Failed pattern match at Data.Either (line 31, column 1 - line 31, column 52): "+[r.constructor.name])}}};var Va=function(t){return function(r){return function(e){if(e instanceof Kt)return t(e.value0);if(e instanceof Yt)return r(e.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},Ep=function(){return Va(T(W.value))(F.create)}();var Tu=function(t){return t};var po={map:function(t){return function(r){return t(r)}}};var qy={apply:function(t){return function(r){return t(r)}},Functor0:function(){return po}},US={bind:function(t){return function(r){return r(t)}},Apply0:function(){return qy}},Km={pure:Tu,Apply0:function(){return qy}},qu={Applicative0:function(){return Km},Bind1:function(){return US}};var Zy=function(t){return Math.min(Math.abs(t),2147483647)},zy=function(t){return function(r){return r===0?0:r>0?Math.floor(t/r):-Math.floor(t/-r)}},Vy=function(t){return function(r){if(r===0)return 0;var e=Math.abs(r);return(t%e+e)%e}},Gy=function(t){return function(r){return t/r}};var Jy={Ring0:function(){return wc}},jy={Ring0:function(){return Jm}};var uu=function(t){return t.mod};var Wl={degree:function(t){return 1},div:Gy,mod:function(t){return function(r){return 0}},CommutativeRing0:function(){return Jy}},so={degree:Zy,div:zy,mod:Vy,CommutativeRing0:function(){return jy}},mo=function(t){return t.div};var Ne={mempty:void 0,Semigroup0:function(){return by}},hf={mempty:"",Semigroup0:function(){return qm}};var Pt=function(t){return t.mempty},tn=function(t){return{mempty:function(r){return Pt(t)},Semigroup0:function(){return Zm(t.Semigroup0())}}};var Ym=function(t){return function(){return t}},Xy=function(t){return function(r){return function(){return r(t())()}}};var ql=function(t){return function(r){return function(){for(var e=0,n=t.length;e<n;e++)r(t[e])()}}};var Qy=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},Wo={Applicative0:function(){return $},Bind1:function(){return Vn}},Vn={bind:Xy,Apply0:function(){return tv(0)}},$={pure:Ym,Apply0:function(){return tv(0)}},Ky=Qy("functorEffect","Effect",function(){return{map:Rl($)}}),tv=Qy("applyEffect","Effect",function(){return{apply:lo(Wo),Functor0:function(){return Ky(0)}}}),S=Ky(20),ot=tv(23),Yy=function(t){return{append:qn(ot)(bt(t))}},Le=function(t){return{mempty:Ym(Pt(t)),Semigroup0:function(){return Yy(t.Semigroup0())}}};var tA=function(t){return function(){return{value:t}}};var he=function(t){return function(){return t.value}},rA=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},An=function(t){return function(r){return function(){r.value=t}}};var ie=tA,VS=rA,Ef=function(t){return VS(function(r){var e=t(r);return{state:e,value:e}})},Nc=function(t){return function(r){return cr(S)(Ef(t)(r))}};var Ma=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),wa=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),qo=function(t){return t.tailRecM};var eA={tailRecM:function(t){return function(r){var e=function(n){if(n instanceof wa)return n.value0;throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 113, column 30 - line 113, column 44): "+[n.constructor.name])};return function(){var a=zn(Vn)(ie)(t(r))();return function(){for(;!function(){var i=he(a)();if(i instanceof Ma){var o=t(i.value0)();return An(o)(a)(),!1}if(i instanceof wa)return!0;throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 104, column 22 - line 109, column 28): "+[i.constructor.name])}(););return{}}(),_(S)(e)(he(a))()}}},Monad0:function(){return Wo}};var nA=function(t){return function(r){return function(){return t(r())}}};function on(t){return function(){return{value:t}}}var Ue=function(t){return function(){return t.value}},aA=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},kn=function(t){return function(r){return function(){return r.value=t}}};var KS=aA,cu=function(t){return KS(function(r){var e=t(r);return{state:e,value:e}})},Su={map:nA};function Lc(){return[]}var ev=function(t){return function(r){return function(){return r.push.apply(r,t)}}};var Tp=function(t){return function(r){return function(e){return function(n){return function(){return n.splice.apply(n,[t,r].concat(e))}}}}};function YS(t){return function(){return t.slice()}}var Sp=YS;var tx=function(){function t(r,e,n,a,u,i){var o,p,s,f,m,v,c;for(o=u+(i-u>>1),o-u>1&&t(r,e,a,n,u,o),i-o>1&&t(r,e,a,n,o,i),p=u,s=o,f=u;p<o&&s<i;)m=a[p],v=a[s],c=e(r(m)(v)),c>0?(n[f++]=v,++s):(n[f++]=m,++p);for(;p<o;)n[f++]=a[p++];for(;s<i;)n[f++]=a[s++]}return function(r){return function(e){return function(n){return function(){return n.length<2||t(r,e,n,n.slice(0),0,n.length),n}}}}}();var Yi=function(t){return ev([t])};var lA=function(t){return function(r){return t&&r}},_A=function(t){return function(r){return t||r}},pA=function(t){return!t};var fu=function(t){return t.not};var tc=function(t){return t.disj},Ga={ff:!1,tt:!0,implies:function(t){return function(r){return tc(Ga)(fu(Ga)(t))(r)}},conj:lA,disj:_A,not:pA};var mA=function(t){return function(r){return function(e){for(var n=r,a=e.length,u=a-1;u>=0;u--)n=t(e[u])(n);return n}}},vA=function(t){return function(r){return function(e){for(var n=r,a=e.length,u=0;u<a;u++)n=t(n)(e[u]);return n}}};var O=function(t){return t.empty};var nt=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),xf=function(t){return function(r){return t(r.value0)(r.value1)}};var cn=function(t){return t.value1};var vi={map:function(t){return function(r){return new nt(r.value0,t(r.value1))}}};var Ja=function(t){return t.value0};var mt=function(t){return t};var Gn=function(){return mt};var fn=Gn,Ee=Gn;var cv=function(){return function(){return function(t){return Gn()}}};var ae=function(t){return t.foldr};var Se=function(t){return function(r){return ae(t)(R(r.Alt0()))(O(r))}},gn=function(t){return function(r){return function(e){return ae(t)(function(){var n=R(r.Alt0());return function(a){return n(e(a))}}())(O(r))}}},ce=function(t){return function(r){return function(e){return ae(r)(function(){var n=Q(t.Apply0());return function(a){return n(e(a))}}())(l(t)(void 0))}}},Jn=function(t){return function(r){return Mt(ce(t)(r))}},$p=function(t){return function(r){return ce(t)(r)(rt(et))}},be=function(t){return t.foldl};var te={foldr:function(t){return function(r){return function(e){if(e instanceof W)return r;if(e instanceof F)return t(e.value0)(r);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof W)return r;if(e instanceof F)return t(r)(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldMap:function(t){return function(r){return function(e){if(e instanceof W)return Pt(t);if(e instanceof F)return r(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[r.constructor.name,e.constructor.name])}}}};var AA=function(t){return function(r){return function(e){return ae(t)(function(n){return function(a){return bt(r.Semigroup0())(e(n))(a)}})(Pt(r))}}},Bt={foldr:mA,foldl:vA,foldMap:function(t){return AA(Bt)(t)}};var Cn=function(t){return t.foldMap};var kA=function(){function t(a){return[a]}function r(a){return function(u){return[a,u]}}function e(a){return function(u){return function(i){return[a,u,i]}}}function n(a){return function(u){return a.concat(u)}}return function(a){return function(u){return function(i){return function(o){return function(p){function s(f,m){switch(m-f){case 0:return i([]);case 1:return u(t)(o(p[f]));case 2:return a(u(r)(o(p[f])))(o(p[f+1]));case 3:return a(a(u(e)(o(p[f])))(o(p[f+1])))(o(p[f+2]));default:var v=f+Math.floor((m-f)/4)*2;return a(u(n)(s(f,v)))(s(v,m))}}return s(0,p.length)}}}}}}();var Rn=function(t){return t.traverse};var OA=function(t){return function(r){return Rn(t)(r)(rt(et))}},Do={traverse:function(t){return kA(Ut(t.Apply0()))(_(t.Apply0().Functor0()))(l(t))},sequence:function(t){return OA(Do)(t)},Functor0:function(){return $r},Foldable1:function(){return Bt}};var r_=function(){return yf(nt.create)}();var gv=function(){return hy};var RA=function(t){return[t]};var e_=function(){return ky(F.create)(W.value)}(),Cv=function(t){return e_(t)(xa(t)-1|0)};var NA=function(){return gy(F.create)(W.value)}();var hv=function(){return Cy(F.create)(W.value)}(),Bp=function(t){return function(r){return function(e){return e.length===0?[]:Gt(e)(function(n){return ca()(hv(n)(e))})(NA(t(r))(e))}}};var nc=function(t){return function(r){return bt(un)([t])(r)}};var LA=function(t){return function(r){for(var e=r.length,n=Array(e),a=0;a<e;a++)n[a]=t(a)(r[a]);return n}};var bo=function(t){return t.mapWithIndex};var di={mapWithIndex:LA,Functor0:function(){return $r}};var zo=function(t){return t.foldrWithIndex};var Vu=function(t){return t.foldlWithIndex};var bi=function(t){return t.foldMapWithIndex};var ac=function(t){return t.traverseWithIndex};var Ao=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var Up=function(t){return function(r){return new Ao(r,O(t))}};var we=function(){function t(){}return t.value=new t,t}(),pr=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),Wp=function(t){return t},FF=function(t){return new pr(t.value0,t.value1)};var OF=function(t){var r=function(e){return function(n){var a=e,u=!1,i;function o(p,s){if(s instanceof pr&&s.value1 instanceof pr&&s.value1.value1 instanceof pr){a=new pr(s,p),n=s.value1.value1.value1;return}var f=function(v){return v instanceof pr&&v.value1 instanceof pr&&v.value1.value1 instanceof we?new pr(t(v.value0),new pr(t(v.value1.value0),we.value)):v instanceof pr&&v.value1 instanceof we?new pr(t(v.value0),we.value):we.value},m=function(v){return function(c){var h=v,ut=!1,dt;function fr(Zt,Kr){if(Zt instanceof pr&&Zt.value0 instanceof pr&&Zt.value0.value1 instanceof pr&&Zt.value0.value1.value1 instanceof pr){h=Zt.value1,c=new pr(t(Zt.value0.value0),new pr(t(Zt.value0.value1.value0),new pr(t(Zt.value0.value1.value1.value0),Kr)));return}return ut=!0,Kr}for(;!ut;)dt=fr(h,c);return dt}};return u=!0,m(p)(f(s))}for(;!u;)i=o(a,n);return i}};return r(we.value)},qp={map:OF};var ja={foldr:function(t){return function(r){var e=function(){var a=function(u){return function(i){var o=u,p=!1,s;function f(m,v){if(v instanceof we)return p=!0,m;if(v instanceof pr){o=new pr(v.value0,m),i=v.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[m.constructor.name,v.constructor.name])}for(;!p;)s=f(o,i);return s}};return a(we.value)}(),n=be(ja)(Mt(t))(r);return function(a){return n(e(a))}}},foldl:function(t){var r=function(e){return function(n){var a=e,u=!1,i;function o(p,s){if(s instanceof we)return u=!0,p;if(s instanceof pr){a=t(p)(s.value0),n=s.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[s.constructor.name])}for(;!u;)i=o(a,n);return i}};return r},foldMap:function(t){return function(r){return be(ja)(function(e){var n=bt(t.Semigroup0())(e);return function(a){return n(r(a))}})(Pt(t))}}};var n_={append:function(t){return function(r){return ae(ja)(pr.create)(r)(t)}}};var Tv={append:function(t){return function(r){return new Ao(t.value0,bt(n_)(t.value1)(FF(r)))}}};var HA={alt:bt(n_),Functor0:function(){return qp}},Sv=function(){return{empty:we.value,Alt0:function(){return HA}}}();var VA=function(t){return t()};var GA=function(t){throw new Error(t)};var JA=function(){return GA};var QF=VA,lu=function(t){return QF(function(){return JA()(t)})};var tr=function(){function t(){}return t.value=new t,t}(),dr=function(){function t(r,e,n,a){this.value0=r,this.value1=e,this.value2=n,this.value3=a}return t.create=function(r){return function(e){return function(n){return function(a){return new t(r,e,n,a)}}}},t}(),Ur=function(){function t(r,e,n,a,u,i,o){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i,this.value6=o}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return function(o){return new t(r,e,n,a,u,i,o)}}}}}}},t}(),oc=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),ki=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),ic=function(){function t(r,e,n,a,u,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return new t(r,e,n,a,u,i)}}}}}},t}(),Go=function(){function t(r,e,n,a,u,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return new t(r,e,n,a,u,i)}}}}}},t}(),cc=function(){function t(r,e,n,a,u,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return new t(r,e,n,a,u,i)}}}}}},t}(),zp=function(){function t(r,e,n,a){this.value0=r,this.value1=e,this.value2=n,this.value3=a}return t.create=function(r){return function(e){return function(n){return function(a){return new t(r,e,n,a)}}}},t}();var XA=function(t){return function(r){return new dr(tr.value,t,r,tr.value)}};var Gp=function(t){return function(r){var e=ur(t),n=function(a){var u=!1,i;function o(p){if(p instanceof tr)return u=!0,W.value;if(p instanceof dr){var s=e(r)(p.value1);if(s instanceof kr)return u=!0,new F(p.value2);if(s instanceof ar){a=p.value0;return}a=p.value3;return}if(p instanceof Ur){var f=e(r)(p.value1);if(f instanceof kr)return u=!0,new F(p.value2);var m=e(r)(p.value4);if(m instanceof kr)return u=!0,new F(p.value5);if(f instanceof ar){a=p.value0;return}if(m instanceof Ar){a=p.value6;return}a=p.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[p.constructor.name])}for(;!u;)i=o(a);return i};return n}};var QA=function(t){return t instanceof tr};var _n=function(t){return function(r){return function(e){var n=t,a=r,u=!1,i;function o(p,s,f){if(s instanceof we)return u=!0,f;if(s instanceof pr){if(s.value0 instanceof oc){n=p,a=s.value1,e=new dr(f,s.value0.value0,s.value0.value1,s.value0.value2);return}if(s.value0 instanceof ki){n=p,a=s.value1,e=new dr(s.value0.value0,s.value0.value1,s.value0.value2,f);return}if(s.value0 instanceof ic){n=p,a=s.value1,e=new Ur(f,s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof Go){n=p,a=s.value1,e=new Ur(s.value0.value0,s.value0.value1,s.value0.value2,f,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof cc){n=p,a=s.value1,e=new Ur(s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5,f);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[s.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[s.constructor.name,f.constructor.name])}for(;!u;)i=o(n,a,e);return i}}},u_=function(t){return function(r){return function(e){var n=function(i){return function(o){var p=i,s=!1,f;function m(v,c){if(v instanceof we)return s=!0,new dr(c.value0,c.value1,c.value2,c.value3);if(v instanceof pr){if(v.value0 instanceof oc)return s=!0,_n(t)(v.value1)(new Ur(c.value0,c.value1,c.value2,c.value3,v.value0.value0,v.value0.value1,v.value0.value2));if(v.value0 instanceof ki)return s=!0,_n(t)(v.value1)(new Ur(v.value0.value0,v.value0.value1,v.value0.value2,c.value0,c.value1,c.value2,c.value3));if(v.value0 instanceof ic){p=v.value1,o=new zp(new dr(c.value0,c.value1,c.value2,c.value3),v.value0.value0,v.value0.value1,new dr(v.value0.value2,v.value0.value3,v.value0.value4,v.value0.value5));return}if(v.value0 instanceof Go){p=v.value1,o=new zp(new dr(v.value0.value0,v.value0.value1,v.value0.value2,c.value0),c.value1,c.value2,new dr(c.value3,v.value0.value3,v.value0.value4,v.value0.value5));return}if(v.value0 instanceof cc){p=v.value1,o=new zp(new dr(v.value0.value0,v.value0.value1,v.value0.value2,v.value0.value3),v.value0.value4,v.value0.value5,new dr(c.value0,c.value1,c.value2,c.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[v.value0.constructor.name,c.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[v.constructor.name,c.constructor.name])}for(;!s;)f=m(p,o);return f}},a=ur(t),u=function(i){return function(o){var p=i,s=!1,f;function m(v,c){if(c instanceof tr)return s=!0,n(v)(new zp(tr.value,r,e,tr.value));if(c instanceof dr){var h=a(r)(c.value1);if(h instanceof kr)return s=!0,_n(t)(v)(new dr(c.value0,r,e,c.value3));if(h instanceof ar){p=new pr(new oc(c.value1,c.value2,c.value3),v),o=c.value0;return}p=new pr(new ki(c.value0,c.value1,c.value2),v),o=c.value3;return}if(c instanceof Ur){var ut=a(r)(c.value1);if(ut instanceof kr)return s=!0,_n(t)(v)(new Ur(c.value0,r,e,c.value3,c.value4,c.value5,c.value6));var dt=a(r)(c.value4);if(dt instanceof kr)return s=!0,_n(t)(v)(new Ur(c.value0,c.value1,c.value2,c.value3,r,e,c.value6));if(ut instanceof ar){p=new pr(new ic(c.value1,c.value2,c.value3,c.value4,c.value5,c.value6),v),o=c.value0;return}if(ut instanceof Ar&&dt instanceof ar){p=new pr(new Go(c.value0,c.value1,c.value2,c.value4,c.value5,c.value6),v),o=c.value3;return}p=new pr(new cc(c.value0,c.value1,c.value2,c.value3,c.value4,c.value5),v),o=c.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[v.constructor.name,c.constructor.name])}for(;!s;)f=m(p,o);return f}};return u(we.value)}}},nO=function(t){return function(r){var e=function(o){return function(p){var s=o,f=!1,m;function v(c,h){if(c instanceof we)return f=!0,h;if(c instanceof pr){if(c.value0 instanceof oc&&c.value0.value2 instanceof tr&&h instanceof tr)return f=!0,_n(t)(c.value1)(new dr(tr.value,c.value0.value0,c.value0.value1,tr.value));if(c.value0 instanceof ki&&c.value0.value0 instanceof tr&&h instanceof tr)return f=!0,_n(t)(c.value1)(new dr(tr.value,c.value0.value1,c.value0.value2,tr.value));if(c.value0 instanceof oc&&c.value0.value2 instanceof dr){s=c.value1,p=new Ur(h,c.value0.value0,c.value0.value1,c.value0.value2.value0,c.value0.value2.value1,c.value0.value2.value2,c.value0.value2.value3);return}if(c.value0 instanceof ki&&c.value0.value0 instanceof dr){s=c.value1,p=new Ur(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3,c.value0.value1,c.value0.value2,h);return}return c.value0 instanceof oc&&c.value0.value2 instanceof Ur?(f=!0,_n(t)(c.value1)(new dr(new dr(h,c.value0.value0,c.value0.value1,c.value0.value2.value0),c.value0.value2.value1,c.value0.value2.value2,new dr(c.value0.value2.value3,c.value0.value2.value4,c.value0.value2.value5,c.value0.value2.value6)))):c.value0 instanceof ki&&c.value0.value0 instanceof Ur?(f=!0,_n(t)(c.value1)(new dr(new dr(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3),c.value0.value0.value4,c.value0.value0.value5,new dr(c.value0.value0.value6,c.value0.value1,c.value0.value2,h)))):c.value0 instanceof ic&&c.value0.value2 instanceof tr&&c.value0.value5 instanceof tr&&h instanceof tr?(f=!0,_n(t)(c.value1)(new Ur(tr.value,c.value0.value0,c.value0.value1,tr.value,c.value0.value3,c.value0.value4,tr.value))):c.value0 instanceof Go&&c.value0.value0 instanceof tr&&c.value0.value5 instanceof tr&&h instanceof tr?(f=!0,_n(t)(c.value1)(new Ur(tr.value,c.value0.value1,c.value0.value2,tr.value,c.value0.value3,c.value0.value4,tr.value))):c.value0 instanceof cc&&c.value0.value0 instanceof tr&&c.value0.value3 instanceof tr&&h instanceof tr?(f=!0,_n(t)(c.value1)(new Ur(tr.value,c.value0.value1,c.value0.value2,tr.value,c.value0.value4,c.value0.value5,tr.value))):c.value0 instanceof ic&&c.value0.value2 instanceof dr?(f=!0,_n(t)(c.value1)(new dr(new Ur(h,c.value0.value0,c.value0.value1,c.value0.value2.value0,c.value0.value2.value1,c.value0.value2.value2,c.value0.value2.value3),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof Go&&c.value0.value0 instanceof dr?(f=!0,_n(t)(c.value1)(new dr(new Ur(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3,c.value0.value1,c.value0.value2,h),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof Go&&c.value0.value5 instanceof dr?(f=!0,_n(t)(c.value1)(new dr(c.value0.value0,c.value0.value1,c.value0.value2,new Ur(h,c.value0.value3,c.value0.value4,c.value0.value5.value0,c.value0.value5.value1,c.value0.value5.value2,c.value0.value5.value3)))):c.value0 instanceof cc&&c.value0.value3 instanceof dr?(f=!0,_n(t)(c.value1)(new dr(c.value0.value0,c.value0.value1,c.value0.value2,new Ur(c.value0.value3.value0,c.value0.value3.value1,c.value0.value3.value2,c.value0.value3.value3,c.value0.value4,c.value0.value5,h)))):c.value0 instanceof ic&&c.value0.value2 instanceof Ur?(f=!0,_n(t)(c.value1)(new Ur(new dr(h,c.value0.value0,c.value0.value1,c.value0.value2.value0),c.value0.value2.value1,c.value0.value2.value2,new dr(c.value0.value2.value3,c.value0.value2.value4,c.value0.value2.value5,c.value0.value2.value6),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof Go&&c.value0.value0 instanceof Ur?(f=!0,_n(t)(c.value1)(new Ur(new dr(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3),c.value0.value0.value4,c.value0.value0.value5,new dr(c.value0.value0.value6,c.value0.value1,c.value0.value2,h),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof Go&&c.value0.value5 instanceof Ur?(f=!0,_n(t)(c.value1)(new Ur(c.value0.value0,c.value0.value1,c.value0.value2,new dr(h,c.value0.value3,c.value0.value4,c.value0.value5.value0),c.value0.value5.value1,c.value0.value5.value2,new dr(c.value0.value5.value3,c.value0.value5.value4,c.value0.value5.value5,c.value0.value5.value6)))):c.value0 instanceof cc&&c.value0.value3 instanceof Ur?(f=!0,_n(t)(c.value1)(new Ur(c.value0.value0,c.value0.value1,c.value0.value2,new dr(c.value0.value3.value0,c.value0.value3.value1,c.value0.value3.value2,c.value0.value3.value3),c.value0.value3.value4,c.value0.value3.value5,new dr(c.value0.value3.value6,c.value0.value4,c.value0.value5,h)))):(f=!0,lu("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[c.constructor.name])}for(;!f;)m=v(s,p);return m}},n=function(o){return function(p){var s=o,f=!1,m;function v(c,h){if(h instanceof dr&&h.value0 instanceof tr&&h.value3 instanceof tr)return f=!0,e(c)(tr.value);if(h instanceof dr){s=new pr(new ki(h.value0,h.value1,h.value2),c),p=h.value3;return}if(h instanceof Ur&&h.value0 instanceof tr&&h.value3 instanceof tr&&h.value6 instanceof tr)return f=!0,e(new pr(new ki(tr.value,h.value1,h.value2),c))(tr.value);if(h instanceof Ur){s=new pr(new cc(h.value0,h.value1,h.value2,h.value3,h.value4,h.value5),c),p=h.value6;return}return f=!0,lu("The impossible happened in partial function `removeMaxNode`.")}for(;!f;)m=v(s,p);return m}},a=function(o){var p=!1,s;function f(m){if(m instanceof dr&&m.value3 instanceof tr)return p=!0,{key:m.value1,value:m.value2};if(m instanceof dr){o=m.value3;return}if(m instanceof Ur&&m.value6 instanceof tr)return p=!0,{key:m.value4,value:m.value5};if(m instanceof Ur){o=m.value6;return}return p=!0,lu("The impossible happened in partial function `maxNode`.")}for(;!p;)s=f(o);return s},u=ur(t),i=function(o){return function(p){var s=o,f=!1,m;function v(c,h){if(h instanceof tr)return f=!0,W.value;if(h instanceof dr){var ut=u(r)(h.value1);if(h.value3 instanceof tr&&ut instanceof kr)return f=!0,new F(new nt(h.value2,e(c)(tr.value)));if(ut instanceof kr){var dt=a(h.value0);return f=!0,new F(new nt(h.value2,n(new pr(new oc(dt.key,dt.value,h.value3),c))(h.value0)))}if(ut instanceof ar){s=new pr(new oc(h.value1,h.value2,h.value3),c),p=h.value0;return}s=new pr(new ki(h.value0,h.value1,h.value2),c),p=h.value3;return}if(h instanceof Ur){var fr=function(){return h.value0 instanceof tr&&h.value3 instanceof tr&&h.value6 instanceof tr}(),ut=u(r)(h.value4),Zt=u(r)(h.value1);if(fr&&Zt instanceof kr)return f=!0,new F(new nt(h.value2,_n(t)(c)(new dr(tr.value,h.value4,h.value5,tr.value))));if(fr&&ut instanceof kr)return f=!0,new F(new nt(h.value5,_n(t)(c)(new dr(tr.value,h.value1,h.value2,tr.value))));if(Zt instanceof kr){var dt=a(h.value0);return f=!0,new F(new nt(h.value2,n(new pr(new ic(dt.key,dt.value,h.value3,h.value4,h.value5,h.value6),c))(h.value0)))}if(ut instanceof kr){var dt=a(h.value3);return f=!0,new F(new nt(h.value5,n(new pr(new Go(h.value0,h.value1,h.value2,dt.key,dt.value,h.value6),c))(h.value3)))}if(Zt instanceof ar){s=new pr(new ic(h.value1,h.value2,h.value3,h.value4,h.value5,h.value6),c),p=h.value0;return}if(Zt instanceof Ar&&ut instanceof ar){s=new pr(new Go(h.value0,h.value1,h.value2,h.value4,h.value5,h.value6),c),p=h.value3;return}s=new pr(new cc(h.value0,h.value1,h.value2,h.value3,h.value4,h.value5),c),p=h.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[h.constructor.name])}for(;!f;)m=v(s,p);return m}};return i(we.value)}},Ia={foldr:function(t){return function(r){return function(e){if(e instanceof tr)return r;if(e instanceof dr)return ae(Ia)(t)(t(e.value2)(ae(Ia)(t)(r)(e.value3)))(e.value0);if(e instanceof Ur)return ae(Ia)(t)(t(e.value2)(ae(Ia)(t)(t(e.value5)(ae(Ia)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof tr)return r;if(e instanceof dr)return be(Ia)(t)(t(be(Ia)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Ur)return be(Ia)(t)(t(be(Ia)(t)(t(be(Ia)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[e.constructor.name])}}},foldMap:function(t){return function(r){return function(e){if(e instanceof tr)return Pt(t);if(e instanceof dr)return bt(t.Semigroup0())(Cn(Ia)(t)(r)(e.value0))(bt(t.Semigroup0())(r(e.value2))(Cn(Ia)(t)(r)(e.value3)));if(e instanceof Ur)return bt(t.Semigroup0())(Cn(Ia)(t)(r)(e.value0))(bt(t.Semigroup0())(r(e.value2))(bt(t.Semigroup0())(Cn(Ia)(t)(r)(e.value3))(bt(t.Semigroup0())(r(e.value5))(Cn(Ia)(t)(r)(e.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[e.constructor.name])}}}},la={foldrWithIndex:function(t){return function(r){return function(e){if(e instanceof tr)return r;if(e instanceof dr)return zo(la)(t)(t(e.value1)(e.value2)(zo(la)(t)(r)(e.value3)))(e.value0);if(e instanceof Ur)return zo(la)(t)(t(e.value1)(e.value2)(zo(la)(t)(t(e.value4)(e.value5)(zo(la)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[e.constructor.name])}}},foldlWithIndex:function(t){return function(r){return function(e){if(e instanceof tr)return r;if(e instanceof dr)return Vu(la)(t)(t(e.value1)(Vu(la)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Ur)return Vu(la)(t)(t(e.value4)(Vu(la)(t)(t(e.value1)(Vu(la)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[e.constructor.name])}}},foldMapWithIndex:function(t){return function(r){return function(e){if(e instanceof tr)return Pt(t);if(e instanceof dr)return bt(t.Semigroup0())(bi(la)(t)(r)(e.value0))(bt(t.Semigroup0())(r(e.value1)(e.value2))(bi(la)(t)(r)(e.value3)));if(e instanceof Ur)return bt(t.Semigroup0())(bi(la)(t)(r)(e.value0))(bt(t.Semigroup0())(r(e.value1)(e.value2))(bt(t.Semigroup0())(bi(la)(t)(r)(e.value3))(bt(t.Semigroup0())(r(e.value4)(e.value5))(bi(la)(t)(r)(e.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[e.constructor.name])}}},Foldable0:function(){return Ia}},KA=function(){return zo(la)(function(t){return function(r){return function(e){return new pr(t,e)}}})(we.value)}();var fc=function(){return tr.value}();var wv=function(t){return function(r){return function(e){return Gt(e)(cn)(nO(t)(r)(e))}}};var lc=function(t){return function(r){return function(e){return function(n){var a=r(Gp(t)(e)(n));if(a instanceof W)return wv(t)(e)(n);if(a instanceof F)return u_(t)(e)(a.value0)(n);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[a.constructor.name])}}}};var aO=function(t){return function(r){return function(e){return function(n){var a=function(u){return function(i){return function(o){return lc(t)(function(){var p=Gt(o)(r(o));return function(s){return F.create(p(s))}}())(u)(i)}}};return Vu(la)(a)(n)(e)}}}};var YA=function(t){return aO(t)(T)};var o_=function(t){return t.partitionMap};var gi=function(t){return t.filterMap};var i_=function(t){return t.filter};var fO=function(t){return t},c_=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),f_=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),lO=function(t){return t},Xp=Gn(),b=lO;var q=function(){return c_.create}();var st=function(){return f_.create}(),ne=function(){var t=_(Wn)(_(S)(T(!0)));return function(r){return fO(t(r))}}(),tt=function(t){return t.attr};function nk(t){return()=>t.slice()}function ak(t){return r=>e=>()=>{e[t]=r}}function uk(t){return()=>t.slice()}var Qp=mt;var G={liftST:Qp,Monad0:function(){return Wo}},St=function(t){return t.liftST};var pn={dimap:function(t){return function(r){return function(e){return function(n){return r(e(t(n)))}}}}},ko=function(t){return t.dimap},Ra=function(t){return function(r){return ko(t)(r)(rt(et))}};var mO=function(t){return function(r){return function(e){return YA(t)(r)(e)}}};var Mv=function(t){return KA(t)};var fk=function(t){return XA(t)(void 0)};var Pv=function(t){return{append:mO(t)}};var lk=function(t){return QA(t)},_k=function(t){return function(r){return function(e){return u_(t)(r)(void 0)(e)}}};var pk={foldMap:function(t){return function(r){var e=Cn(ja)(t)(r);return function(n){return e(Mv(n))}}},foldl:function(t){return function(r){var e=be(ja)(t)(r);return function(n){return e(Mv(n))}}},foldr:function(t){return function(r){var e=ae(ja)(t)(r);return function(n){return e(Mv(n))}}}};var Iv=fc;var sk=function(t){return{mempty:Iv,Semigroup0:function(){return Pv(t)}}};var Kp=function(t){return function(r){return function(e){return wv(t)(r)(e)}}};function mk(t){return function(r){return function(){return setTimeout(r,t)}}}function vk(t){return function(){clearTimeout(t)}}var Yp=mk;var DO={eq:function(t){return function(r){return t===r}}},ts={compare:function(t){return function(r){return ur(Ye)(t)(r)}},Eq0:function(){return DO}};var __=vk;var Ci=function(t){return t.sampleOn};var xe=function(t){return t.keepLatest};var Gu=function(t){return t.fold};var p_=function(t){return function(r){return function(e){return function(n){return gi(t.Filterable1())(cn)(Gu(t)(function(a){return function(u){return _(vi)(l(Uo))(r(a)(u.value0))}})(e)(new nt(n,W.value)))}}}},rs=function(t){return function(r){var e=function(n){return function(a){if(a instanceof W)return new F({now:n,last:W.value});if(a instanceof F)return new F({now:n,last:new F(a.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 54, column 3 - line 54, column 50): "+[n.constructor.name,a.constructor.name])}};return gi(t.Filterable1())(rt(et))(Gu(t)(e)(r)(W.value))}},s_=function(t){return t.fix};var Nn=function(t){return function(r){return function(e){return Ut(t.Alternative0().Applicative0().Apply0())(_(t.Filterable1().Functor1())(ji)(r))(e)}}};var Dk=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),dk=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),bk=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),yk=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),ft={map:function(t){return function(r){return function(e,n,a,u){return r(e,n,a,function(i){return u(t(i))})}}}},qc={apply:function(t){return function(r){return function(e,n,a,u){return a(function(i){return t(e,n,a,function(o){return a(function(p){return r(e,n,a,function(s){return u(o(s))})})})})}}},Functor0:function(){return ft}},go={bind:function(t){return function(r){return function(e,n,a,u){return a(function(i){return t(e,n,a,function(o){var p=r(o);return a(function(s){return p(e,n,a,function(f){return u(f)})})})})}}},Apply0:function(){return qc}};var D={pure:function(t){return function(r,e,n,a){return a(t)}},Apply0:function(){return qc}},dO={Applicative0:function(){return D},Bind1:function(){return go}};var _t=function(t){var r=function(e){var n=!1,a;function u(i){var o=i(void 0);if(o instanceof Dk){e=o.value0;return}if(o instanceof dk)return n=!0,_(S)(Ma.create)(o.value1);if(o instanceof bk)return n=!0,_(S)(Ma.create)(Qp(o.value0));if(o instanceof yk)return n=!0,l($)(new wa(o.value0));throw new Error("Failed pattern match at Hyrule.Zora (line 131, column 15 - line 135, column 30): "+[o.constructor.name])}for(;!n;)a=u(e);return a};return qo(eA)(r)(function(e){return t(function(n,a){return new dk(n,a)},bk.create,Dk.create,yk.create)})},Nv=function(t){return function(r,e,n,a){return e(_(Su)(function(u){return function(i){return a(u)}})(t))}},V={liftST:Nv,Monad0:function(){return dO}};var es=function(t){return function(r){return function(e,n,a,u){return e(function(i){return u(Pt(t))},_(S)(function(i){return function(o){return u(i)}})(r))}}};function Lv(t){return function(r){return t===r}}var ns=Lv;var kO=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},gO=function(t){return t};var _u=function(t){return ko(pn)(_(Wn)(es(Ne)))(function(){var r=_(ft)(_t);return function(e){return _t(r(e))}}())(t)},CO=function(t){return function(r){return function(e){return function(n){return function(a){return P(t.Monad0().Bind1())(St(t)(on(W.value)))(function(u){return P(t.Monad0().Bind1())(e(function(i){return St(t)(cr(Su)(kn(new F(i))(u)))}))(function(i){return P(t.Monad0().Bind1())(n(function(o){return P(t.Monad0().Bind1())(St(t)(Ue(u)))(ce(r)(te)(function(p){return a(o(p))}))}))(function(o){return l(r)(Q(r.Apply0())(i)(o))})})})}}}}},g={map:function(t){return function(r){return function(e){return r(function(n){return e(t(n))})}}}};var hO=function(t){return function(r){return function(e){return function(n){return function(a){return P(t.Monad0().Bind1())(St(t)(on(n)))(function(u){return e(function(i){return P(t.Monad0().Bind1())(St(t)(cu(r(i))(u)))(a)})})}}}}},m_=function(t){return function(r){return function(e){return function(n){return e(function(a){var u=r(a);if(u instanceof F)return n(u.value0);if(u instanceof W)return l(t)(void 0);throw new Error("Failed pattern match at FRP.Event (line 189, column 13 - line 191, column 27): "+[u.constructor.name])})}}}},Bv=function(t){return function(r){return m_(t)(function(e){var n=r(e);if(n)return new F(e);if(!n)return W.value;throw new Error("Failed pattern match at FRP.Event (line 117, column 13 - line 119, column 25): "+[n.constructor.name])})}},kk=function(t){return{compact:m_(t)(rt(et)),separate:function(r){return{left:m_(t)(function(e){if(e instanceof Kt)return new F(e.value0);if(e instanceof Yt)return W.value;throw new Error("Failed pattern match at FRP.Event (line 100, column 13 - line 102, column 33): "+[e.constructor.name])})(r),right:m_(t)(function(e){if(e instanceof Yt)return new F(e.value0);if(e instanceof Kt)return W.value;throw new Error("Failed pattern match at FRP.Event (line 107, column 13 - line 109, column 32): "+[e.constructor.name])})(r)}}}},Fu=function(t){return{filter:Bv(t),filterMap:m_(t),partition:function(r){return function(e){return{yes:Bv(t)(r)(e),no:Bv(t)(function(){var n=fu(Ga);return function(a){return n(r(a))}}())(e)}}},partitionMap:function(r){return function(e){return{left:gi(Fu(t))(function(){var n=Va(F.create)(T(W.value));return function(a){return n(r(a))}}())(e),right:gi(Fu(t))(function(n){return Ep(r(n))})(e)}}},Compactable0:function(){return kk(t)},Functor1:function(){return g}}},EO=function(t){return function(r){return function(e){return function(n){return function(a){return P(t.Monad0().Bind1())(St(t)(on(W.value)))(function(u){return P(t.Monad0().Bind1())(St(t)(Lc))(function(i){return P(t.Monad0().Bind1())(St(t)(on(W.value)))(function(o){return P(t.Monad0().Bind1())(St(t)(Lc))(function(p){return P(t.Monad0().Bind1())(St(t)(on(!0)))(function(s){return P(t.Monad0().Bind1())(e(function(f){return P(t.Monad0().Bind1())(St(t)(Ue(s)))(function(m){return m?St(t)(cr(Su)(Yi(f)(i))):P(t.Monad0().Bind1())(St(t)(kn(new F(f))(u)))(function(){return P(t.Monad0().Bind1())(St(t)(Ue(o)))(ce(r)(te)(function(v){return a(v(f))}))})})}))(function(f){return P(t.Monad0().Bind1())(n(function(m){return P(t.Monad0().Bind1())(St(t)(Ue(s)))(function(v){return v?St(t)(cr(Su)(Yi(m)(p))):P(t.Monad0().Bind1())(St(t)(kn(new F(m))(o)))(function(){return P(t.Monad0().Bind1())(St(t)(Ue(u)))(ce(r)(te)(function(c){return a(m(c))}))})})}))(function(m){return P(t.Monad0().Bind1())(St(t)(kn(!1)(s)))(function(){return P(t.Monad0().Bind1())(St(t)(Sp(i)))(function(v){return P(t.Monad0().Bind1())(St(t)(Sp(p)))(function(c){return Xr(Qr)(t.Monad0().Bind1())(function(){return v.length===0?cr(r.Apply0().Functor0())(St(t)(kn(Cv(c))(o))):Jn(r)(Bt)(v)(function(h){return P(t.Monad0().Bind1())(St(t)(kn(new F(h))(u)))(function(){return Jn(r)(Bt)(c)(function(ut){return P(t.Monad0().Bind1())(St(t)(kn(new F(ut))(o)))(function(){return a(ut(h))})})})})}())(function(){return P(t.Monad0().Bind1())(St(t)(Tp(0)(xa(v))([])(i)))(function(){return P(t.Monad0().Bind1())(St(t)(Tp(0)(xa(c))([])(p)))(function(){return l(r)(Q(r.Apply0())(f)(m))})})})})})})})})})})})})})}}}}},$t=function(t){return function(r){return r}(as(277).subscribe)(t)},Ht=function(t){return function(r){return r}(as(300).makeEvent)(t)},Rf=function(t){return function(r){return Xr(Qr)(t.Monad0().Bind1())(l(t.Monad0().Applicative0())(void 0))(function(){return function(e){return e(t)(r)}(as(316).create)})}},as=kO("backdoor","FRP.Event",function(){return{makeEvent:gO,create:function(){var t=function(r){return function(e){return P(r.Monad0().Bind1())(St(r)(on([])))(function(n){return l(r.Monad0().Applicative0())({event:function(a){return P(e.Monad0().Bind1())(St(e)(cu(function(u){return bt(un)(u)([a])})(n)))(function(){return l(e.Monad0().Applicative0())(P(e.Monad0().Bind1())(St(e)(cu(Bp(ns)(a))(n)))(function(){return l(e.Monad0().Applicative0())(void 0)}))})},push:function(a){return P(e.Monad0().Bind1())(St(e)(Ue(n)))(ce(e.Monad0().Applicative0())(Bt)(function(u){return u(a)}))}})})}};return t}(),subscribe:function(){var t=function(r){return function(e){return r(e)}};return t}(),bus:function(){var t=function(r){return function(e){return Ht(function(n){return P(r.Monad0().Bind1())(Rf(r)(r))(function(a){return Xr(Qr)(r.Monad0().Bind1())(n(e(a.push)(a.event)))(function(){return l(r.Monad0().Applicative0())(l(r.Monad0().Applicative0())(void 0))})})})}};return t}(),memoize:function(){var t=function(r){return function(e){return function(n){return Ht(function(a){return P(r.Monad0().Bind1())(Rf(r)(r))(function(u){return Xr(Qr)(r.Monad0().Bind1())(a(n(u.event)))(function(){return $t(e)(u.push)})})})}}};return t}(),hot:function(){var t=function(r){return function(e){return P(r.Monad0().Bind1())(Rf(r)(r))(function(n){return P(r.Monad0().Bind1())($t(e)(n.push))(function(a){return l(r.Monad0().Applicative0())({event:n.event,unsubscribe:a})})})}};return t}(),mailboxed:function(){var t=function(r){return function(e){return function(n){return function(a){return Ht(function(u){return P(e.Monad0().Bind1())(St(e)(on(fc)))(function(i){return Xr(Qr)(e.Monad0().Bind1())(u(a(function(o){return Ht(function(p){return Xr(Qr)(e.Monad0().Bind1())(cr(e.Monad0().Bind1().Apply0().Functor0())(St(e)(cu(lc(r)(function(s){if(s instanceof W)return new F([p]);if(s instanceof F)return new F(bt(un)(s.value0)([p]));throw new Error("Failed pattern match at FRP.Event (line 484, column 21 - line 486, column 55): "+[s.constructor.name])})(o))(i))))(function(){return l(e.Monad0().Applicative0())(cr(e.Monad0().Bind1().Apply0().Functor0())(St(e)(cu(lc(r)(function(s){if(s instanceof W)return W.value;if(s instanceof F)return new F(Bp(ns)(p)(s.value0));throw new Error("Failed pattern match at FRP.Event (line 493, column 21 - line 495, column 69): "+[s.constructor.name])})(o))(i))))})})})))(function(){return P(e.Monad0().Bind1())($t(n)(function(o){return P(e.Monad0().Bind1())(St(e)(Ue(i)))(function(p){var s=Gp(r)(o.address)(p);if(s instanceof W)return l(e.Monad0().Applicative0())(void 0);if(s instanceof F)return Jn(e.Monad0().Applicative0())(Bt)(s.value0)(function(f){return f(o.payload)});throw new Error("Failed pattern match at FRP.Event (line 502, column 13 - line 504, column 49): "+[s.constructor.name])})}))(function(o){return l(e.Monad0().Applicative0())(Xr(Qr)(e.Monad0().Bind1())(cr(e.Monad0().Bind1().Apply0().Functor0())(St(e)(kn(fc)(i))))(function(){return o}))})})})})}}}};return t}(),delay:function(){var t=function(r){return function(e){return Ht(function(n){return function(){var u=ie(Pt(sk(ts)))(),i=$t(e)(function(o){return function(){var s=ie(W.value)(),f=Yp(r)(function(){n(o)();var v=he(s)();return Gt(l($)(void 0))(function(c){return Nc(Kp(ts)(c))(u)})(v)()})();return An(new F(f))(s)(),Nc(bt(Pv(ts))(fk(f)))(u)()}})();return function(){var p=he(u)();return Jn($)(pk)(p)(__)(),i()}}})}};return t}()}}),Hv=as(417),Uv=function(t){return function(r){return function(e){return e(t)}(Hv.bus)(r)}},Ju=function(t){return function(r){return r}(Hv.delay)(t)};var _c=function(t){return function(r){return function(e){return e(t)}(Hv.memoize)(r)}},TO=function(t){return function(r){return function(e){return function(n){return P(r.Bind1())(Rf(t)(t))(function(a){var u=e(a.event);return P(r.Bind1())($t(u.input)(a.push))(function(i){return P(r.Bind1())($t(u.output)(n))(function(o){return l(r.Applicative0())(Q(r.Bind1().Apply0())(i)(o))})})})}}}},SO=function(t){return function(r){return function(e){return P(t.Monad0().Bind1())(St(t)(on(W.value)))(function(n){return P(t.Monad0().Bind1())(r(function(a){return Xr(Qr)(t.Monad0().Bind1())(P(t.Monad0().Bind1())(St(t)(Ue(n)))($p(t.Monad0().Applicative0())(te)))(function(){return P(t.Monad0().Bind1())($t(a)(e))(function(u){return St(t)(cr(Su)(kn(new F(u))(n)))})})}))(function(a){return l(t.Monad0().Applicative0())(Xr(Qr)(t.Monad0().Bind1())(P(t.Monad0().Bind1())(St(t)(Ue(n)))($p(t.Monad0().Applicative0())(te)))(function(){return a}))})})}}},xO=function(t){return{apply:function(r){return function(e){return EO(t)(t.Monad0().Applicative0())(r)(_(g)(ji)(e))}},Functor0:function(){return g}}};var C=function(t){return{pure:function(r){return function(e){return df(t.Monad0().Bind1().Apply0().Functor0())(l(t.Monad0().Applicative0())(void 0))(e(r))}},Apply0:function(){return xO(t)}}};var N=function(t){return{alt:function(r){return function(e){return function(n){return Ut(t.Apply0())(_(t.Apply0().Functor0())(function(a){return function(u){return Q(t.Apply0())(a)(u)}})(r(n)))(e(n))}}},Functor0:function(){return g}}};var E=function(t){return{empty:function(r){return l(t)(l(t)(void 0))},Alt0:function(){return N(t)}}},FO=function(t){return{Applicative0:function(){return C(t)},Plus1:function(){return E(t.Monad0().Applicative0())}}},Jt=function(t){return{fold:hO(t),keepLatest:SO(t),sampleOn:CO(t)(t.Monad0().Applicative0()),fix:TO(t)(t.Monad0()),Alternative0:function(){return FO(t)},Filterable1:function(){return Fu(t.Monad0().Applicative0())}}};function v_(t,r){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);for(var a in t)({}).hasOwnProperty.call(t,a)&&(e[a]=t[a]);return e}var Ck=function(t){return function(){return function(){return function(r){return function(e){return function(n){return fo(ge(t)(r))(e)(n)}}}}}};var hk=function(){return function(){return function(t){return function(r){return v_(t,r)}}}},Zc=function(t){return function(){return function(){return function(r){return function(e){return function(n){return fo(ge(t)(r))(e)(n)}}}}}},hi=function(t){return function(){return function(r){return function(e){return Za(ge(t)(r))(e)}}}};var jn={vb:function(t){return function(r){return function(e){return function(n){return l(t.Monad0().Applicative0())(new nt({},{}))}}}}},us=function(t){return t.vb},PO={vbus:function(){var t=function(){return function(e){return function(n){return function(a){return function(u){return Ht(function(i){return P(e.Monad0().Bind1())(us(n)(e)(d.value)(d.value)(d.value))(function(o){return Xr(Qr)(e.Monad0().Bind1())(i(u(o.value0)(o.value1)))(function(){return l(e.Monad0().Applicative0())(l(e.Monad0().Applicative0())(void 0))})})})}}}}},r=function(){return function(e){return function(n){return t()(e)(n)}}};return r}()},qv=function(){return function(t){return function(r){return function(e){return function(n){return n()(t)(r)}(PO.vbus)(e)}}}},ju=function(t){return function(){return function(){return function(){return function(r){return function(e){return function(){return function(){return{vb:function(n){return function(a){return function(u){return function(i){return P(n.Monad0().Bind1())(us(e)(n)(d.value)(u)(i))(function(o){return P(n.Monad0().Bind1())(us(r)(n)(d.value)(u)(i))(function(p){return l(n.Monad0().Applicative0())(new nt(Zc(t)()()(d.value)(p.value0)(o.value0),Zc(t)()()(d.value)(p.value1)(o.value1)))})})}}}}}}}}}}}}},fe=function(t){return function(){return function(){return function(r){return function(){return function(){return{vb:function(e){return function(n){return function(a){return function(u){return P(e.Monad0().Bind1())(us(r)(e)(d.value)(a)(u))(function(i){return P(e.Monad0().Bind1())(Rf(e)(e))(function(o){return l(e.Monad0().Applicative0())(new nt(Zc(t)()()(d.value)(o.push)(i.value0),Zc(t)()()(d.value)(o.event)(i.value1)))})})}}}}}}}}}}};var Ei=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Zv=function(){function t(){}return t.value=new t,t}();var Nf=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Lf=function(){function t(){}return t.value=new t,t}(),zv=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),os=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Bf=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),mc=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),M=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Ek=function(t){return t};var D_={eq:function(t){return function(r){return t instanceof Ei&&r instanceof Ei?t.value0===r.value0:t instanceof Zv&&r instanceof Zv}}};var U=function(t){return new Bf(t)};var is=function(t){return new os(t)};var Tk=t=>r=>r[t];var jo=function(t){return t.reflectType};var xk=function(t){return jo(t)};var d_=$r;var zc=function(){return function(t){return t}};var wk=function(t){return[t]};var Mk=function(){return function(){return function(){return function(){return function(){return function(t){var r=xk(t);return function(e){return Tk(r(e))}}}}}}};var Vv=[];var Vc=function(){return function(){return function(t){return function(r){return nc(t)(r)}}}};function Pk(t){return function(){var r={};for(var e in t)hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}}var Xo={};function Gv(t){return t()}function Ik(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(t[n]));return e}function Rk(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(n)(t[n]));return e}function Nk(t){return function(r){return function(e){return function(n){var a=e;function u(o){return function(p){return r(p)(o)(n[o])}}for(var i in n)hasOwnProperty.call(n,i)&&(a=t(a)(u(i)));return a}}}}function b_(t){return function(r){var e=[];for(var n in r)hasOwnProperty.call(r,n)&&e.push(t(n)(r[n]));return e}}var BO=Object.keys||b_(function(t){return function(){return t}});function Jv(t){return function(r){return function(e){return function(){return e[t]=r,e}}}}var jv=function(t){return function(r){return function(){return delete r[t],r}}};var Xv=b_(function(t){return function(r){return r}});var VO=Pk;var Bk=function(t){return function(r){return Gv(function(){var n=VO(r)();return t(n)(),n})}};var Hk=function(t){return function(r){return Rk(r,t)}};var Ti=function(t){return function(r){return Bk(Jv(t)(r))}},ls={map:function(t){return function(r){return Ik(r,t)}}},GO={mapWithIndex:Hk,Functor0:function(){return ls}},Qv=function(){return mt};var _s=Nk(ji),Uk=function(t){return function(r){return _s(function(e){return function(n){return function(a){return bt(t.Semigroup0())(e)(r(n)(a))}}})(Pt(t))}},y_={foldl:function(t){return _s(function(r){return function(e){return t(r)}})},foldr:function(t){return function(r){return function(e){return ae(Bt)(t)(r)(Xv(e))}}},foldMap:function(t){return function(r){return Uk(t)(T(r))}}},Wk={foldlWithIndex:function(t){return _s(Mt(t))},foldrWithIndex:function(t){return function(r){return function(e){return ae(Bt)(xf(t))(r)(b_(nt.create)(e))}}},foldMapWithIndex:function(t){return Uk(t)},Foldable0:function(){return y_}},JO={traverseWithIndex:function(t){return function(r){return function(e){return _s(function(n){return function(a){return function(u){return Ut(t.Apply0())(_(t.Apply0().Functor0())(Mt(Ti(a)))(n))(r(a)(u))}}})(l(t)(Xo))(e)}}},FunctorWithIndex0:function(){return GO},FoldableWithIndex1:function(){return Wk},Traversable2:function(){return Hf}},Hf={traverse:function(t){var r=ac(JO)(t);return function(e){return r(T(e))}},sequence:function(t){return Rn(Hf)(t)(rt(et))},Functor0:function(){return ls},Foldable1:function(){return y_}};var Kv=function(t){return Bk(jv(t))};var qk=function(){function t(){}return t.value=new t,t}(),Yv=function(){function t(){}return t.value=new t,t}(),jO=function(){function t(){}return t.value=new t,t}();var Zk=function(t){return function(r){return function(e){var n=function(a){var u=function(i){return function(o){return new nt(o+1|0,new nt(i,o))}};return p_(Jt(t))(u)(a)(0)};return new os(xe(Jt(t))(_c(t)(n(e))(function(a){return _(g)(function(u){return R(N(t.Monad0().Applicative0()))(l(C(t))(new Nf(r(u.value0))))(_(g)(T(Lf.value))(i_(Fu(t.Monad0().Applicative0()))(function(){var i=nr(Xi)(u.value1+1|0);return function(o){return i(cn(o))}}())(a)))})(a)})))}}};var Qu=function(t){return function(r){return function(e){return function(n){return function(a){var u=function(i){return i(n)(a)};return function(i){if(i instanceof Bf)return gn(Bt)(E(t))(Qu(t)(r)(e)(n)(a))(i.value0);if(i instanceof mc)return xe(Jt(r))(_(g)(Qu(t)(r)(e)(n)(a))(i.value0));if(i instanceof M)return u(e.toElt(i.value0));if(i instanceof os)return Ht(function(o){return P(r.Monad0().Bind1())(St(r)(on(Xo)))(function(p){return P(r.Monad0().Bind1())($t(i.value0)(function(s){return P(r.Monad0().Bind1())(e.ids(a))(function(f){return P(r.Monad0().Bind1())(St(r)(on(l(t)(void 0))))(function(m){return P(r.Monad0().Bind1())(e.ids(a))(function(v){return P(r.Monad0().Bind1())(St(r)(on(l(t)(void 0))))(function(c){return P(r.Monad0().Bind1())(St(r)(on([])))(function(h){return P(r.Monad0().Bind1())(St(r)(on(l(t)(void 0))))(function(ut){return P(r.Monad0().Bind1())(_(t.Apply0().Functor0())(Ei.create)(e.ids(a)))(function(dt){return P(r.Monad0().Bind1())(St(r)(on(qk.value)))(function(fr){return P(r.Monad0().Bind1())($t(s)(function(Zt){return P(r.Monad0().Bind1())(St(r)(Ue(fr)))(function(Kr){return Zt instanceof zv&&Kr instanceof Yv?P(r.Monad0().Bind1())(St(r)(Ue(h)))(ce(t)(Bt)(function(){var re=e.doLogic(Zt.value0)(a);return function(pt){return o(re(pt))}}())):Zt instanceof Lf&&Kr instanceof Yv?Xr(Qr)(r.Monad0().Bind1())(cr(t.Apply0().Functor0())(St(r)(kn(jO.value)(fr))))(function(){var re=Q(t.Apply0())(Q(t.Apply0())(Q(t.Apply0())(Q(t.Apply0())(P(r.Monad0().Bind1())(St(r)(Ue(h)))(ce(t)(Bt)(function(pt){return Jn(t)(te)(n.parent)(function(mr){return o(e.disconnectElement(a)({id:pt,parent:mr,scope:dt}))})})))(eu(r.Monad0().Bind1())(St(r)(Ue(m)))))(eu(r.Monad0().Bind1())(St(r)(Ue(c)))))(cr(t.Apply0().Functor0())(St(r)(cu(Kv(f))(p)))))(cr(t.Apply0().Functor0())(St(r)(cu(Kv(v))(p))));return Q(t.Apply0())(cr(t.Apply0().Functor0())(St(r)(kn(re)(ut))))(re)}):Zt instanceof Nf&&Kr instanceof qk?Xr(Qr)(r.Monad0().Bind1())(cr(t.Apply0().Functor0())(St(r)(kn(Yv.value)(fr))))(function(){return P(r.Monad0().Bind1())($t(Qu(t)(r)(e)(function(){var re={};for(var pt in n)({}).hasOwnProperty.call(n,pt)&&(re[pt]=n[pt]);return re.scope=dt,re.raiseId=function(mr){return cr(t.Apply0().Functor0())(St(r)(cu(bt(un)([mr]))(h)))},re}())(a)(Zt.value0))(o))(function(re){return Xr(Qr)(r.Monad0().Bind1())(cr(t.Apply0().Functor0())(St(r)(cu(Ti(v)(re))(p))))(function(){return cr(t.Apply0().Functor0())(St(r)(kn(re)(c)))})})}):l(t)(void 0)})}))(function(Zt){return Xr(Qr)(r.Monad0().Bind1())(cr(t.Apply0().Functor0())(St(r)(kn(Zt)(m))))(function(){return Xr(Qr)(r.Monad0().Bind1())(cr(t.Apply0().Functor0())(St(r)(cu(Ti(f)(Zt))(p))))(function(){return eu(r.Monad0().Bind1())(St(r)(Ue(ut)))})})})})})})})})})})})}))(function(s){return l(t)(Xr(Qr)(r.Monad0().Bind1())(P(r.Monad0().Bind1())(St(r)(Ue(p)))(be(y_)(Q(t.Apply0()))(l(t)(void 0))))(function(){return s}))})})});throw new Error("Failed pattern match at Bolson.Control (line 531, column 17 - line 615, column 20): "+[i.constructor.name])}}}}}},XO=function(){return function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){var o=function(p){return function(s){return Ht(function(f){return P(t.Monad0().Bind1())(St(t)(nk(_($r)(T(""))(zc()(u)))))(function(m){var v=Se(Bt)(E(t.Monad0().Applicative0()))(bo(di)(function(c){return Gm(Ey)(function(h){return function(ut){return ut instanceof M?function(dt){return dt(function(){var fr={};for(var Zt in p)({}).hasOwnProperty.call(p,Zt)&&(fr[Zt]=p[Zt]);return fr.parent=W.value,fr.scope=e(p.scope),fr.raiseId=function(Kr){return St(t)(ak(c)(Kr)(m))},fr}())(s)}(a.toElt(ut.value0)):h(a.wrapElt(ut))}})})(zc()(u)));return P(t.Monad0().Bind1())($t(v)(f))(function(c){return P(t.Monad0().Bind1())(St(t)(on(l(t.Monad0().Applicative0())(void 0))))(function(h){return P(t.Monad0().Bind1())(_(t.Monad0().Bind1().Apply0().Functor0())(mt)(St(t)(uk(m))))(function(ut){var dt=_(d_)(function(Zt){return function(Kr){return new M(a.fromEltO1(function(re){return function(pt){return Ht(function(mr){return Xr(Qr)(t.Monad0().Bind1())(re.raiseId(Zt))(function(){return Xr(Qr)(t.Monad0().Bind1())(Jn(t.Monad0().Applicative0())(te)(re.parent)(function(Fr){return mr(a.giveNewParent(pt)({id:Zt,parent:Fr,scope:re.scope})(Kr))}))(function(){return l(t.Monad0().Applicative0())(l(t.Monad0().Applicative0())(void 0))})})})}}))}})(ut),fr=Qu(t.Monad0().Applicative0())(t)(n)(p)(s)(i(dt)(mt));return P(t.Monad0().Bind1())($t(fr)(f))(function(Zt){return Xr(Qr)(t.Monad0().Bind1())(cr(t.Monad0().Bind1().Apply0().Functor0())(St(t)(kn(Zt)(h))))(function(){return l(t.Monad0().Applicative0())(Xr(Qr)(t.Monad0().Bind1())(c)(function(){return Xr(Qr)(t.Monad0().Bind1())(Zn(t.Monad0().Applicative0())(!r)(Jn(t.Monad0().Applicative0())(Bt)(zc()(ut))(function(Kr){return f(a.deleteFromCache(s)({id:Kr}))})))(function(){return eu(t.Monad0().Bind1())(St(t)(Ue(h)))})}))})})})})})})})}};return new M(a.fromEltO2(o))}}}}}}}};var tD=function(){return function(t){return function(r){return function(e){return function(n){return function(a){return XO()(t)(!1)(rt(et))(r)(e)(n)(a)}}}}}};var zk=function(t){return function(r){return function(e){return function(n){var a=function(u){return function(i){return Ht(function(o){return P(t.Monad0().Bind1())(St(t)(on(W.value)))(function(p){var s=n(new M(e.fromElt(function(f){return function(m){return Ht(function(v){return Xr(Qr)(t.Monad0().Bind1())(P(t.Monad0().Bind1())(St(t)(Ue(p)))(function(c){if(c instanceof W)return l(t.Monad0().Applicative0())(void 0);if(c instanceof F)return Jn(t.Monad0().Applicative0())(te)(f.parent)(function(h){return Zn(t.Monad0().Applicative0())(c.value0!==h)(Q(t.Monad0().Bind1().Apply0())(f.raiseId(c.value0))(v(e.connectToParent(i)({id:c.value0,parent:h}))))});throw new Error("Failed pattern match at Bolson.Control (line 641, column 36 - line 648, column 16): "+[c.constructor.name])}))(function(){return l(t.Monad0().Applicative0())(l(t.Monad0().Applicative0())(void 0))})})}})));return $t(Qu(t.Monad0().Applicative0())(t)(r)(function(){var f={};for(var m in u)({}).hasOwnProperty.call(u,m)&&(f[m]=u[m]);return f.parent=u.parent,f.scope=u.scope,f.raiseId=function(v){return Xr(Qr)(t.Monad0().Bind1())(u.raiseId(v))(function(){return cr(t.Monad0().Bind1().Apply0().Functor0())(St(t)(kn(new F(v))(p)))})},f}())(i)(s))(o)})})}};return new M(e.fromElt(a))}}}};var a$=function(){return function(){return function(){return function(t){return function(r){return function(e){return Pl(e.type)(t)?Za(e.type)(t)(e.value):r(e)}}}}}};var le=function(){return function(t){return function(r){return function(e){return{type:ge(t)(r),value:e}}}}};var jk=function(t){return lu("Data.Variant: pattern match failure ["+(t.type+"]"))},Ve=function(){return function(){return function(){return function(t){return a$()()()(t)(jk)}}}};function Xk(t){var r={};for(var e in t)({}).hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}function Qk(t){return function(r){return function(e){return e[t]=r,e}}}function Kk(t){return function(r){return function(e){return e[t]=r(e[t]),e}}}var A_=si;var Yk=function(){return function(){return function(t){return function(r){return function(e){return function(n){return Kk(ge(t)(r))(e)(n)}}}}}};var eD=function(){return function(){return function(t){return function(r){return function(e){return function(n){return Qk(ge(t)(r))(e)(n)}}}}}};var k_=et,ss=function(t){return function(r){return t(Xk(r))}},tg=Mt(ss)({});var i$=function(t){return t},c$=function(t){return t.mappingWithIndex};var f$=function(t){return t.mapping};var Xn={mapRecordWithIndexBuilder:function(t){return function(r){return rt(k_)}}},rg=function(t){return t.mapRecordWithIndexBuilder},Wr=function(t){return function(r){return function(e){return function(){return function(){return{mapRecordWithIndexBuilder:function(n){return function(a){return Hu(A_)(Yk()()(t)(d.value)(c$(r)(a)(d.value)))(rg(e)(d.value)(a))}}}}}}}};var pu=function(){return function(t){return{hmap:function(){var r=rg(t)(d.value);return function(e){return ss(r(i$(e)))}}()}}};var aD=function(t){return t.hmap},qr=function(t){return{mappingWithIndex:function(r){return function(e){return f$(t)(r)}}}};var eg=function(){function t(){}return t.value=new t,t}();var Yu=function(t){return{mapping:function(r){return aD(t)(eg.value)}}},_e={mapping:function(t){return _(Wn)(_t)}};var l$=function(t){return aD(t)(eg.value)},to=function(){return function(){return function(t){return function(r){return function(e){return function(n){return new mc(qv()(V)(r)(e)(Ra(pn)(l$(pu()(t)))(n)))}}}}}};var ng=function(t){return Uv(V)(Ra(pn)(_(Wn)(_t))(t))};var rn=function(t){return new mc(ng(t))};var s$=function(t){return t.makeText},m$=function(t){return function(r){return function(e){return _(g)(function(n){return t.setText(function(a){return{id:r,text:a}}(n))})(e)}}},v$=function(t){return function(r){return function(e){return _(g)(function(n){return function(a){if(a.value instanceof c_)return t.setProp({id:r,key:a.key,value:a.value.value0});if(a.value instanceof f_)return t.setCb({id:r,key:a.key,value:a.value.value0});throw new Error("Failed pattern match at Deku.Control (line 88, column 26 - line 90, column 45): "+[a.value.constructor.name])}(Xp(n))})(e)}}},D$=function(t){return t.makeElement},d$=function(t){return t.attributeParent},sn=function(t){var r=function(e){return function(n){return Ht(function(a){return P(go)(n.ids)(function(u){return Xr(Qr)(go)(e.raiseId(u))(function(){return _(ft)(Q(qc)(a(n.deleteFromCache({id:u}))))($t(Se(Bt)(E(D))([l(C(V))(s$(n)({id:u,parent:e.parent,scope:e.scope})),m$(n)(u)(t)]))(a))})})})}};return new M(r)},pe=function(t){return sn(l(C(V))(t))},ag=function(t){return function(r){return function(e){return t(O(E(D)))([Zk(V)(r)(e)])}}};var b$=function(t){return{doLogic:function(r){return function(e){return function(n){return e.sendToPos({id:n,pos:r})}}},ids:function(){var r=Ee(t);return function(e){return function(n){return n.ids}(r(e))}}(),disconnectElement:function(r){return function(e){return r.disconnectElement({id:e.id,scope:e.scope,parent:e.parent,scopeEq:nr(D_)})}},toElt:function(r){return r}}};var ug=Qu(D)(V)(b$()),og=function(t){return function(r){return function(e){return Ht(function(n){return P(go)(e.ids)(function(a){return $t(R(N(D))(l(C(V))(e.makeRoot({id:a,root:t})))(ug({parent:new F(a),scope:new Ei("rootScope"),raiseId:function(u){return l(D)(void 0)},pos:W.value})(e)(r)))(n)})})}}};var X=function(t){return function(r){return function(e){var n=function(a){return function(u){return Ht(function(i){return P(go)(u.ids)(function(o){return Xr(Qr)(go)(a.raiseId(o))(function(){return _(ft)(function(p){return Q(qc)(i(u.deleteFromCache({id:o})))(p)})($t(R(N(D))(Se(Bt)(E(D))(bt(un)([l(C(V))(D$(u)({id:o,parent:a.parent,scope:a.scope,tag:t})),v$(u)(o)(r)])(Gt([])(function(p){return[l(C(V))(d$(u)({id:o,parent:p,pos:a.pos}))]})(a.parent))))(ug({parent:new F(o),scope:a.scope,raiseId:function(p){return l(D)(void 0)},pos:W.value})(u)(e)))(i))})})})}};return n}}};var De=function(){function t(){}return t.value=new t,t}();var ye={attr:function(t){return function(r){return b({key:"click",value:st(r)})}}};var Xt=function(){function t(){}return t.value=new t,t}();var ms={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var ig={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var Dt={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var cg={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}},Uf={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var uD={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var fg={attr:function(t){return function(r){return b({key:"style",value:q(r)})}}};var oD=function(t){return function(r){return new M(X("a")(t)(U(r)))}};var hr=function(t){return function(r){return new M(X("div")(t)(U(r)))}},Zr=hr(O(E(D)));var qf=function(t){return function(r){return new M(X("span")(t)(U(r)))}},iD=qf(O(E(D)));var _g=(t,r,e,n)=>{t(a=>n.units[a].main.appendChild(n.units[r].main))(e)};var pg=t=>r=>()=>{r.units[t.id].main.parentNode||(typeof t.pos.value0=="number"&&r.units[t.parent].main.children[t.pos.value0]?r.units[t.parent].main.insertBefore(r.units[t.id].main,r.units[t.parent].main.children[t.pos.value0]):t.parent.indexOf("@!%")!==-1?r.units[t.parent].main.parentNode.replaceChild(r.units[t.id].main,r.units[t.parent].main):r.units[t.parent].main.appendChild(r.units[t.id].main))},sg=t=>r=>e=>()=>{var n,a=r.id;e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(a),e.hydrating&&t&&r.parent.value0&&(n=document.body.querySelectorAll("[data-deku-ssr-"+a+"]").item(0))?e.units[a]={listeners:{},parent:r.parent,scope:r.scope,main:n}:e.units[a]={listeners:{},parent:r.parent,scope:r.scope,main:document.createElement(r.tag)}},mg=t=>r=>e=>n=>()=>{var a=e.id,u;if(n.scopes[e.scope]||(n.scopes[e.scope]=[]),n.scopes[e.scope].push(a),n.hydrating&&t&&e.parent.value0&&(u=document.body.querySelectorAll("[data-deku-ssr-"+e.parent.value0+"]").item(0))){var i=0;if(u.childNodes.length===1)u.prepend(document.createTextNode(""));else for(var i=0;i<u.childNodes.length;i++)if(u.childNodes[i].nodeType===8&&u.childNodes[i].nodeValue===a){i=i-1;break}n.units[a]={main:u.childNodes[i],parent:e.parent,scope:e.scope}}else n.units[a]={main:document.createTextNode(""),parent:e.parent,scope:e.scope},_g(r,a,e.parent,n)};function cD(){return{units:{},scopes:{}}}var vg=t=>r=>e=>()=>{var n=r.id,a=r.value;e.hydrating&&t&&!e.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),e.units[n].main.tagName==="INPUT"&&r.key==="value"?e.units[n].main.value=a:e.units[n].main.tagName==="INPUT"&&r.key==="checked"?e.units[n].main.checked=a==="true":e.units[n].main.setAttribute(r.key,a)},Dg=t=>r=>e=>()=>{var n=r.id,a=r.value;if(e.hydrating&&t&&!e.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),r.key==="@self@")a(e.units[n].main)();else{e.units[n].listeners[r.key]&&e.units[n].main.removeEventListener(r.key,e.units[n].listeners[r.key]);var u=i=>a(i)();e.units[n].main.addEventListener(r.key,u),e.units[n].listeners[r.key]=u}},dg=t=>r=>()=>{var e=t.id;r.units[e].main.nodeValue=t.text},bg=t=>r=>e=>n=>()=>{var a,u,i=e.id,o=e.html,p=e.verb,s=e.cache,f=e.parent,m=e.scope,v=e.pxScope;if(n.hydrating&&t&&e.parent.value0&&(a=document.body.querySelectorAll("[data-deku-ssr-"+i+"]").item(0)))n.units[i]={listeners:{},scope:m,parent:f,main:a};else{let h=Object.entries(s);for(var c=0;c<h.length;c++){let ut=h[c][0];h[c][1]===!0?o=o.replace(p+ut+p,'data-deku-attr-internal="'+ut+'"'):o=o.replace(p+ut+p,'<span style="display:contents;" data-deku-elt-internal="'+ut+'"></span>')}u=document.createElement("div"),u.innerHTML=o.trim(),n.units[i]={listeners:{},scope:m,parent:f,main:u.firstChild}}n.scopes[m]||(n.scopes[m]=[]),n.scopes[m].push(i),u||(u=a),u.querySelectorAll("[data-deku-attr-internal]").forEach(function(h){var ut=h.getAttribute("data-deku-attr-internal");let dt=ut+"@!%"+v;n.units[dt]={listeners:{},main:h,scope:m},n.scopes[m].push(dt)}),u.querySelectorAll("[data-deku-elt-internal]").forEach(function(h){var ut=h.getAttribute("data-deku-elt-internal");let dt=ut+"@!%"+v;n.units[ut+"@!%"+v]={listeners:{},main:h,scope:m},n.scopes[m].push(dt)}),a||_g(r,i,f,n)},yg=t=>r=>()=>{var e=t.id;r.units[e]={main:t.root}},Ag=t=>r=>()=>{var e=t.id,n=t.parent;r.units[e].containingScope=t.scope,r.units[n].main.prepend(r.units[e].main)},kg=t=>r=>()=>{var e=t.id;r.units[e].noop||r.units[e].containingScope&&!t.scopeEq(r.units[e].containingScope)(t.scope)||r.units[e].main.remove()},gg=t=>r=>()=>{delete r.units[t.id]},Cg=t=>r=>()=>{var e=t.id,n=t.pos,a=r.units[e].main.parentNode;a.insertBefore(r.units[e].main,a.children.length<=n?a.children[a.children.length-1]:n<0?a.children[0]:a.children[n])};var hg=function(t){return function(r){return function(e){return(e|0)===e?t(e):r}}},jr=function(t){return t};var fD=function(t){return function(r){return Math.pow(t,r)|0}};var vs=isFinite;var g_=Math.floor;var vc=function(t){return function(r){return Math.pow(t,r)}},C_=function(t){return function(r){return t%r}},Ds=Math.round;var ds=Math.sin;var Dc=3.141592653589793;var Zf=function(){return hg(F.create)(W.value)}(),Tg=function(t){if(!vs(t))return 0;if(t>=jr(ua(Pc)))return ua(Pc);if(t<=jr(oa(Pc)))return oa(Pc);if(oe)return ya(0)(Zf(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},Sg=function(t){return Tg(Ds(t))};var h_=function(t){return Tg(g_(t))};var eo=Math.random;var E_=function(t){return function(r){return function(){var n=eo(),a=(jr(r)-jr(t)+1)*n+jr(t);return h_(a)}}};var xg=function(t){return t};var T$=1,bs=2147483647,S$=function(){return bs-1|0}(),Vf=function(t){var r=function(e){return function(n){return function(a){var u=n-e|0,i=uu(so)(a)(u),o=i<e;return o?i+n|0:i}}};return r(T$)(S$)(t)};var x$=0,F$=48271,Fg=function(t){return function(r){return ca()(Zf(C_(jr(F$)*jr(r)+jr(t))(jr(bs))))}},Og=Fg(x$);var N$=function(){function t(i){this.fn=i}var r={},e=function(i,o){this.head=i,this.tail=o};function n(i){return new e(i,r)}function a(i){return function(o){return new e(i,o)}}function u(i){for(var o=[],p=i;p!==r;)o.push(p.head),p=p.tail;return o}return function(i){return function(o){return function(p){var s=function(m,v){return i(o(a)(p(m)))(v)},f=function(m,v,c){if(v===0)return m;var h=c[v-1];return new t(function(){var ut=f(s(h,m),v-1,c);return ut})};return function(m){for(var v=o(n)(p(m[m.length-1])),c=f(v,m.length-1,m);c instanceof t;)c=c.fn();return o(u)(c)}}}}}();var Ig=function(t){return t};var Rg=un;var Ng=Bt;var Hg=Ig,S_=function(t){return t};var x_=function(t){return Hg(RA(t))};var Gf=function(t){if(xa(t)>0)return new F(Hg(t));if(oe)return W.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 157, column 1 - line 157, column 58): "+[t.constructor.name])};var Ug=function(t){return function(r){return t(S_(r))}};var Wg=Ug(xa);var qg=function(){return Ug(gv())};var Si=function(t){return t.state};function Yo(t){return new Error(t)}function jc(t){return function(){throw t}}function gs(t){return function(r){return function(){try{return r()}catch(e){return e instanceof Error||Object.prototype.toString.call(e)==="[object Error]"?t(e)():t(new Error(e.toString()))()}}}}var ho=function(t){return t.throwError};var Dw={throwError:jc,Monad0:function(){return Wo}};var yD={catchError:Mt(gs),MonadThrow0:function(){return Dw}};var xi=function(t){return t.catchError};var O_=function(t){return function(r){return xi(t)(_(t.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(Yt.create)(r))(function(){var e=l(t.MonadThrow0().Monad0().Applicative0());return function(n){return e(Kt.create(n))}}())}};var se={liftEffect:rt(et),Monad0:function(){return Wo}},Ae=function(t){return t.liftEffect};var xs=function(t){return{map:function(r){return function(e){return function(n){return _(t)(function(a){return new nt(r(a.value0),a.value1)})(e(n))}}}}};var AD=function(t){return{Applicative0:function(){return $s(t)},Bind1:function(){return Fs(t)}}},Fs=function(t){return{bind:function(r){return function(e){return function(n){return P(t.Bind1())(r(n))(function(a){var u=e(a.value0);return u(a.value1)})}}},Apply0:function(){return Os(t)}}},Os=function(t){return{apply:lo(AD(t)),Functor0:function(){return xs(t.Bind1().Apply0().Functor0())}}},$s=function(t){return{pure:function(r){return function(e){return l(t.Applicative0())(new nt(r,e))}},Apply0:function(){return Os(t)}}};var kD=function(t){return{state:function(r){var e=l(t.Applicative0());return function(n){return e(r(n))}},Monad0:function(){return AD(t)}}};var Qg=function(t){return function(r){var e=t(r);return e.value0}};var Cw=function(t){return t};var tC=function(){var t=function(r){return new nt(xg(r.newSeed),function(){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);return e.newSeed=Og(r.newSeed),e}())};return Si(kD(qu))(t)}();var Eo=xs(po),rC=_(Eo)(function(t){return jr(t)/jr(bs)})(tC);var Xf=function(t){return Qg(Cw(t))};var Qc=Fs(qu);var Kc=Os(qu),Yg=function(t){return function(r){var e=jr(r),n=jr(t),a=function(o){return n+C_(o)(e-n+1)},u=_(Eo)(jr)(tC),i=Ut(Kc)(_(Eo)(Br(Fa))(u))(_(Eo)(In(Fa)(2))(u));return _(Eo)(function(o){return h_(a(o))})(i)}},CD=function(t){return function(r){var e=t<=r;return e?Yg(t)(r):Yg(r)(t)}};var I_=$s(qu);var hD=function(t){return P(Qc)(CD(0)(Wg(t)-1|0))(function(r){return l(I_)(qg()(t)(r))})};var N_=function(t){return t.arbitrary};var eC={arbitrary:rC};var nC=function(){return{arbitrary:CD(-1e6)(1e6)}}();var aC=function(t){return{ids:es(hf)(function(){var e=he(t)(),n=jt(au)(Xf(N_(nC))({newSeed:Vf(e),size:5}));return cr(S)(Ef(Br(Uu)(1))(t))(),n}),makeElement:sg(!1),attributeParent:pg,makeRoot:yg,makeText:mg(!1)(Gt(void 0)),makePursx:bg(!1)(Gt(void 0)),setProp:vg(!1),setCb:Dg(!1),setText:dg,sendToPos:Cg,deleteFromCache:gg,giveNewParent:Ag,disconnectElement:kg}};var Fi=function(){return window};function oC(t,r,e,n){if(typeof window<"u"){var a=window[e];if(a!=null&&n instanceof a)return r(n)}for(var u=n;u!=null;){var i=Object.getPrototypeOf(u),o=i.constructor.name;if(o===e)return r(n);if(o==="Object")return t;u=i}return t}var Rt=function(t){return function(r){return oC(W.value,F.create,t,r)}};function iC(t,r,e){return t==null?r:e(t)}var en=function(t){return iC(t,W.value,F.create)};var TD=Rt("HTMLCanvasElement");function _C(t){return function(){return t.body}}var pC=function(){var t=_(S)(en);return function(r){return t(_C(r))}}();var sC=mt;function Yc(t){return function(){return t.valueAsNumber}}var Qf=Rt("HTMLInputElement");function OD(t){return function(){return t.document}}function Ps(t){return function(r){return function(){return r.requestAnimationFrame(t)}}}var $D=mt;var MM=function(t){return function(r){return function(){var n=cD(),a=Vr(S)(ie(0))(function(){var u=og(t)(r);return function(i){return u(aC(i))}}())();return $t(_u(a))(function(u){return u(n)})()}}};var PM=function(t){return function(){var e=P(Vn)(P(Vn)(Fi)(OD))(pC)();return Gt(Pt(Le(Le(Ne))))(function(n){return MM(n)(t)})(_(Re)(sC)(e))()}},yC=function(t){return cr(S)(PM(t))};var RM=function(t){return t};var Y={pursxToElement:function(t){return function(r){return function(e){return{cache:Xo,element:function(n){return function(a){return O(E(D))}}}}}}},wD=function(t){return t.pursxToElement},mn=function(){return function(t){return function(r){return function(e){return{pursxToElement:function(n){return function(a){return function(u){var i=wD(t)(n)(d.value)(u);return{cache:Ti(jo(r)(d.value))(!0)(i.cache),element:function(o){return function(p){return R(N(D))(_(g)(Ra(pn)(Xp)(function(s){if(s.value instanceof c_)return p.setProp({id:jo(r)(d.value)+("@!%"+n),key:s.key,value:s.value.value0});if(s.value instanceof f_)return p.setCb({id:jo(r)(d.value)+("@!%"+n),key:s.key,value:s.value.value0});throw new Error("Failed pattern match at Deku.Pursx (line 4191, column 38 - line 4201, column 24): "+[s.value.constructor.name])}))(hi(e)()(d.value)(u)))(i.element(o)(p))}}}}}}}}}}};var B=RM,Dr=function(t){return function(r){return function(){return function(){return function(e){return function(n){return function(a){return function(u){var i=function(o){return function(p){return Ht(function(s){return P(go)(p.ids)(function(f){return P(go)(p.ids)(function(m){return Xr(Qr)(go)(o.raiseId(f))(function(){var v=wD(e)(m)(d.value)(u);return _(ft)(Q(qc)(s(p.deleteFromCache({id:f}))))($t(R(N(D))(l(C(V))(p.makePursx({id:f,parent:o.parent,cache:v.cache,pxScope:m,scope:o.scope,html:jo(t)(a),verb:jo(r)(n)})))(v.element(o)(p)))(s))})})})})}};return new M(i)}}}}}}}},It=function(t){return function(){return function(){return function(r){return Dr(t)({reflectType:function(){return"~"}})()()(r)(d.value)}}}};var NM=Qu(D)(V)({doLogic:function(t){return function(r){return function(e){return r.sendToPos({id:e,pos:t})}}},ids:function(){var t=Ee();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:nr(D_)})}},toElt:function(t){return t}}),H=function(){return function(t){return function(r){return function(e){return{pursxToElement:function(n){return function(a){return function(u){var i=hi(e)()(d.value)(u),o=wD(t)(n)(d.value)(u);return{cache:Ti(jo(r)(d.value))(!1)(o.cache),element:function(p){return function(s){return R(N(D))(NM({parent:new F(jo(r)(d.value)+("@!%"+n)),scope:p.scope,raiseId:function(f){return l(D)(void 0)},pos:p.pos})(s)(i))(o.element(p)(s))}}}}}}}}}}};var gt=function(){return function(){return{defaults:Mt(hk()())}}},LM=function(t){return t.defaults},Ct={convertRecordOptions:function(t){return function(r){return function(e){return rt(k_)}}}},AC=function(t){return t.convertRecordOptions},Aa=function(t){return t.convertOptionsWithDefaults},ht=function(){return function(t){return{convertOptions:function(r){return function(e){return tg(AC(t)(r)(d.value)(e))}}}}},BM=function(t){return t.convertOptions},Et=function(t){return function(r){return{convertOptionsWithDefaults:function(e){return function(n){var a=LM(r)(n),u=BM(t)(e);return function(i){return a(u(i))}}}}}},HM=function(t){return t.convertOption},J=function(t){return function(r){return function(){return function(){return function(){return function(e){return{convertRecordOptions:function(n){return function(a){return function(u){return Hu(A_)(eD()()(e)(d.value)(HM(r)(n)(d.value)(hi(e)()(d.value)(u))))(AC(t)(n)(d.value)(u))}}}}}}}}}};var MD=function(){var t=Up(Sv);return function(r){return Wp(t(r))}}();var Out=typeof Array.from=="function",$ut=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",wut=typeof String.prototype.fromCodePoint=="function",Mut=typeof String.prototype.codePointAt=="function";var Oi={proof:function(t){return t},Coercible0:function(){}},ID=function(t){return t.proof};var wu=void 0;var Ws=function(t){return t.toInt},hC=function(t){return function(r){return Ws(t)(wu)}};var su={toInt:function(t){return 8}},EC={Nat0:function(){return su}},ti={toInt:function(t){return 7}},TC={Nat0:function(){return ti}},ri={toInt:function(t){return 6}},SC={Nat0:function(){return ri}},La={toInt:function(t){return 5}},qs={Nat0:function(){return La}},Qn={toInt:function(t){return 4}},sa={Nat0:function(){return Qn}},Kn={toInt:function(t){return 3}},Mu={Nat0:function(){return Kn}},Yn={toInt:function(t){return 2}},Pu={Nat0:function(){return Yn}},ta={toInt:function(t){return 1}},Iu={Nat0:function(){return ta}},Me={toInt:function(t){return 0}};var wr=function(t){return function(){return function(r){return function(){return function(e){return{Nat0:r.Nat1,Pos1:function(){return t}}}}}}};var To={Nat0:function(){return ti},Nat1:function(){return su}};var So={Nat0:function(){return ri},Nat1:function(){return su}};var xo={Nat0:function(){return La},Nat1:function(){return su}};var Fo={Nat0:function(){return Qn},Nat1:function(){return su}};var ka={Nat0:function(){return Qn},Nat1:function(){return La}};var Oo={Nat0:function(){return Kn},Nat1:function(){return su}};var ga={Nat0:function(){return Kn},Nat1:function(){return La}};var $o={Nat0:function(){return Yn},Nat1:function(){return su}};var Ca={Nat0:function(){return Yn},Nat1:function(){return La}};var wo={Nat0:function(){return ta},Nat1:function(){return su}};var ha={Nat0:function(){return ta},Nat1:function(){return La}};var Mo={Nat0:function(){return Me},Nat1:function(){return su}};var Ea={Nat0:function(){return Me},Nat1:function(){return La}};var xC={Nat0:function(){return Me},Nat1:function(){return su}};var RD={Nat0:function(){return Me},Nat1:function(){return ti}};var ND={Nat0:function(){return Me},Nat1:function(){return ri}};var B_={Nat0:function(){return Me},Nat1:function(){return La}};var Ka={Nat0:function(){return Me},Nat1:function(){return Qn}};var Tn={Nat0:function(){return Me},Nat1:function(){return Kn}};var Sn={Nat0:function(){return Me},Nat1:function(){return Yn}};var xn={Nat0:function(){return Me},Nat1:function(){return ta}},Ru={Nat0:function(){return Me},Nat1:function(){return Me}};var FC=Do;var Zs=function(t){return t};var H_=function(t){return function(){return function(r){return function(e){return r[Ws(t)(e)]}}}};var zs=function(t){return function(r){var e=hC(t)(d.value),n=function(){return e===0?[]:yn(0)(e-1|0)}();return _($r)(r)(n)}};var no=[];var Mr=function(t){return function(r){return function(e){return nc(r)(e)}}};var Fn={first:function(t){return function(r){return new nt(t(r.value0),r.value1)}},second:_(vi),Profunctor0:function(){return pn}},ra=function(t){return t.second},Vs=function(t){return t.first};var dP=function(t){return function(r){return function(e){return function(n){return ko(e)(t)(r)(n)}}}};var MC=function(){return function(){return function(t){return dP(Gn())(Gn())(t)}}};var PC=function(){return function(){return function(t){return MC()()(t)}}};var AP=function(t){return function(r){return function(e){return ko(r.Profunctor0())(t)(function(n){return n.value1(n.value0)})(Vs(r)(e))}}},IC=function(t){return function(r){return function(e){return AP(function(n){return new nt(t(n),function(a){return r(n)(a)})})(e)}}};var RC=function(t){return function(){return function(){return function(r){return function(e){return IC(hi(t)()(r))(Mt(Ck(t)()()(r)))(e)}}}}};var NC=function(t){return t};var TP=JSON.parse;var SP=JSON.stringify;var Gs=function(t){return t};var Js=function(t){return t};var js=function(t){return function(r){return t(r)}},U_=function(t){return{map:function(r){return js(_(t)(_(Rc)(r)))}}};var HD=function(t){return{Applicative0:function(){return W_(t)},Bind1:function(){return UD(t)}}},UD=function(t){return{bind:function(r){return function(e){return P(t.Bind1())(r)(Va(function(){var n=l(t.Applicative0());return function(a){return n(Kt.create(a))}}())(function(n){var a=e(n);return a}))}},Apply0:function(){return BC(t)}}},BC=function(t){return{apply:lo(HD(t)),Functor0:function(){return U_(t.Bind1().Apply0().Functor0())}}},W_=function(t){return{pure:function(){var r=l(t.Applicative0());return function(e){return Gs(r(Yt.create(e)))}}(),Apply0:function(){return BC(t)}}};var HC=function(t){return{throwError:function(){var r=l(t.Applicative0());return function(e){return Gs(r(Kt.create(e)))}}(),Monad0:function(){return HD(t)}}};var WD=function(t){return function(r){return{alt:function(e){return function(n){return P(r.Bind1())(e)(function(a){if(a instanceof Yt)return l(r.Applicative0())(new Yt(a.value0));if(a instanceof Kt)return P(r.Bind1())(n)(function(u){if(u instanceof Yt)return l(r.Applicative0())(new Yt(u.value0));if(u instanceof Kt)return l(r.Applicative0())(new Kt(bt(t)(a.value0)(u.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[u.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[a.constructor.name])})}},Functor0:function(){return U_(r.Bind1().Apply0().Functor0())}}}};var qD=function(){var t=Ee();return function(r){return t(Js(r))}}();function q_(t){return Object.prototype.toString.call(t).slice(8,-1)}var WC=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var VD=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var qC=mt;var GD=function(t){var r=ho(HC(t));return function(e){return r(MD(e))}};var JD=function(t){return function(r){return function(e){if(q_(e)===r)return l(W_(t))(qC(e));if(oe)return GD(t)(new VD(r,q_(e)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[r.constructor.name,e.constructor.name])}}};var jD=function(t){return JD(t)("String")};var Ks=function(){function t(){}return t.value=new t,t}(),Ys=function(){function t(){}return t.value=new t,t}(),zC=function(){function t(){}return t.value=new t,t}(),VC=function(){function t(){}return t.value=new t,t}(),QD=function(){function t(){}return t.value=new t,t}(),GC=function(){function t(){}return t.value=new t,t}(),JC=function(){function t(){}return t.value=new t,t}();var jC=function(t){return t},XC=function(t){return t};var QC=function(t){return t};var KC=function(t){return t};var YC=function(t){return t};var th=function(t){return t},rh=function(t){return t},eh=function(t){return t},nh=function(t){return t},ah=function(t){return t};var KD=function(){function t(){}return t.value=new t,t}(),uh=function(){function t(){}return t.value=new t,t}(),oh=function(){function t(){}return t.value=new t,t}(),YD=function(){function t(){}return t.value=new t,t}(),ih=function(){function t(){}return t.value=new t,t}();var tm=function(t){return t};var rl=function(t){return t};var zP=function(t){return t},Z_=function(t){return t};var ef={toAudioOnOff:rt(et)};var nf=function(t){return t.toAudioParameter},ch=function(t){return t.toAudioOnOff},fh=function(){return Nf.create}(),lh=function(){return Lf.value}();var rm=function(){return NC(function(){var t=PC()()(pn),r=RC({reflectSymbol:function(){return"o"}})()()(d.value)(Fn);return function(e){return t(r(e))}}())},_h=mt;var VP=function(){var t=le()({reflectSymbol:function(){return"unit"}})(d.value);return function(r){return Z_(t(r))}}();var GP=function(t){return function(r){return{toAudioParameter:function(e){return VP(e)}}}},ph=function(t){return function(r){return{toAudioParameter:function(){var e=nf(GP(t)(r));return function(n){return e(zP(function(a){return{u:a}}(n)))}}()}}},sh=function(){return le()({reflectSymbol:function(){return"2x"}})(d.value)(void 0)}(),mh=function(){var t=le()({reflectSymbol:function(){return"sudden"}})(d.value);return function(r){return Z_(t(r))}}();var vh={toAudioParameter:mh},em={toAudioParameter:function(t){return mh({n:t})}},td=function(){return le()({reflectSymbol:function(){return"step"}})(d.value)(void 0)}();var rd=function(){return le()({reflectSymbol:function(){return"on"}})(d.value)(void 0)}(),z_={x:rd,o:0},vt=function(){return l(C(G))(fn()(le()({reflectSymbol:function(){return"onOff"}})(d.value)(z_)))};var Dh=function(){return le()({reflectSymbol:function(){return"off"}})(d.value)(void 0)}();var JP=function(){var t=le()({reflectSymbol:function(){return"numeric"}})(d.value);return function(r){return Z_(t(r))}}();var We={toAudioParameter:JP};var ei=function(){return le()({reflectSymbol:function(){return"linear"}})(d.value)(void 0)}();var dh=function(){return le()({reflectSymbol:function(){return"exponential"}})(d.value)(void 0)}(),jP=function(){var t=le()({reflectSymbol:function(){return"envelope"}})(d.value);return function(r){return Z_(t(r))}}();var Bn={toAudioParameter:jP},XP=function(){var t=le()({reflectSymbol:function(){return"cancel"}})(d.value);return function(r){return Z_(t(r))}}();var bh={toAudioParameter:XP};var QP=function(){function t(){}return t.value=new t,t}(),KP=function(){function t(){}return t.value=new t,t}(),YP=function(){function t(){}return t.value=new t,t}(),tI=function(){function t(){}return t.value=new t,t}(),rI=function(){function t(){}return t.value=new t,t}(),eI=function(){function t(){}return t.value=new t,t}(),nI=function(){function t(){}return t.value=new t,t}(),aI=function(){function t(){}return t.value=new t,t}(),uI=function(){function t(){}return t.value=new t,t}(),oI=function(){function t(){}return t.value=new t,t}(),iI=function(){function t(){}return t.value=new t,t}(),cI=function(){function t(){}return t.value=new t,t}(),fI=function(){function t(){}return t.value=new t,t}(),lI=function(){function t(){}return t.value=new t,t}(),$i=function(t){return{toPeriodicOscSpec:function(r){return le()({reflectSymbol:function(){return"realImg"}})(d.value)({real:Zs(r.value0),img:Zs(r.value1)})}}};var nm={toInitializeTriangleOsc:function(t){return ah(function(r){return{frequency:r}}(t))}};var yh={toInitializeStereoPanner:function(t){return nh(function(r){return{pan:r}}(t))}};var el={toInitializeSquareOsc:function(t){return eh(function(r){return{frequency:r}}(t))}};var Ac={toInitializeSinOsc:function(t){return rh(function(r){return{frequency:r}}(t))}};var Ah={toInitializeSawtoothOsc:function(t){return th(function(r){return{frequency:r}}(t))}};var ed={toInitializeRecorder:function(t){return jC(function(r){return{cb:r}}(t))}};var V_={toInitializeMicrophone:function(t){return XC(function(r){return{microphone:r}}(t))}};var kh=function(t){return function(r){return{toInitializeIIRFilter:function(e){return function(n){return function(a){return{feedforward:ID(Oi)(Gn()(e.value0)),feedback:ID(Oi)(Gn()(e.value1))}}}}}}};var lt={toInitializeGain:function(t){return YC(function(r){return{gain:r}}(t))}};var gh={toInitializeConvolver:function(t){return QC(function(r){return{buffer:r}}(t))}},am={toInitializeConstant:function(t){return KC(function(r){return{offset:r}}(t))}};var _I={convertOption:function(t){return function(r){return rt(et)}}},G_={convertOption:function(t){return function(r){return rt(et)}}},Ch={convertOption:function(t){return function(r){return rt(et)}}},hh={convertOption:function(t){return function(r){return F.create}}},Eh={convertOption:function(t){return function(r){return rt(et)}}},wi={convertOption:function(t){return function(r){return rt(et)}}},nl={convertOption:function(t){return function(r){return rt(et)}}},al={convertOption:function(t){return function(r){return rt(et)}}},ul={convertOption:function(t){return function(r){return rt(et)}}},ol={convertOption:function(t){return function(r){return rt(et)}}},il={convertOption:function(t){return function(r){return rt(et)}}},Th={convertOption:function(t){return function(r){return rt(et)}}},Sh={convertOption:function(t){return function(r){return rt(et)}}},xh={convertOption:function(t){return function(r){return rt(et)}}},nd={convertOption:function(t){return function(r){return rt(et)}}},af={convertOption:function(t){return function(r){return rt(et)}}},J_={convertOption:function(t){return function(r){return rt(et)}}},j_={convertOption:function(t){return function(r){return rt(et)}}};var cl={convertOption:function(t){return function(r){return rt(et)}}},Fh={convertOption:function(t){return function(r){return rt(et)}}},Oh={convertOption:function(t){return function(r){return rt(et)}}},$h={convertOption:function(t){return function(r){return rt(et)}}},ad={convertOption:function(t){return function(r){return rt(et)}}};var wh={convertOption:function(t){return function(r){return rt(et)}}},ud={convertOption:function(t){return function(r){return rt(et)}}},On={convertOption:function(t){return function(r){return rt(et)}}},vn={convertOption:function(t){return function(r){return rt(et)}}},od={convertOption:function(t){return function(r){return rt(et)}}},um={convertOption:function(t){return function(r){return rt(et)}}},pI=function(t){return t.toPeriodicOscSpec},Mi=function(t){return{convertOption:function(r){return function(e){return pI(t)}}}},id=function(t){return t.toInitializeWaveShaper},Mh=function(t){return t.toInitializeTriangleOsc},Ph=function(t){return t.toInitializeStereoPanner},Ih=function(t){return t.toInitializeSquareOsc},Rh=function(t){return t.toInitializeSinOsc},Nh=function(t){return t.toInitializeSawtoothOsc},Lh=function(t){return t.toInitializeRecorder},cd=function(t){return t.toInitializePlayBuf},Bh=function(t){return t.toInitializePeriodicOsc},Hh=function(t){return t.toInitializePeaking},Uh=function(t){return t.toInitializeNotch},Wh=function(t){return t.toInitializeMicrophone},qh=function(t){return t.toInitializeLowshelf},fd=function(t){return t.toInitializeLowpass},ld=function(t){return t.toInitializeLoopBuf},Zh=function(t){return t.toInitializeIIRFilter},zh=function(t){return t.toInitializeHighshelf},_d=function(t){return t.toInitializeHighpass},Vh=function(t){return t.toInitializeGain},Gh=function(t){return t.toInitializeDynamicsCompressor},pd=function(t){return t.toInitializeDelay},Jh=function(t){return t.toInitializeConvolver},jh=function(t){return t.toInitializeConstant},sd=function(t){return t.toInitializeBandpass},md=function(t){return t.toInitializeAllpass};var sI={oversample:sh},mI=function(t){return{toInitializeWaveShaper:function(r){return Aa(t)(QP.value)(sI)(r)}}},Xh={toInitializeWaveShaper:function(){var t=id(mI(Et(ht()(J(Ct)(_I)()()()({reflectSymbol:function(){return"curve"}})))(gt()())));return function(r){return t(function(e){return{curve:e}}(r))}}()},vI=function(){return{bufferOffset:0,playbackRate:1,duration:W.value}}(),X_=function(t){return{toInitializePlayBuf:function(r){return Aa(t)(KP.value)(vI)(r)}}},Ya={toInitializePlayBuf:function(){var t=cd(X_(Et(ht()(J(Ct)(G_)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())));return function(r){return t(function(e){return{buffer:e}}(r))}}()},DI={},Pi=function(t){return{toInitializePeriodicOsc:function(r){return Aa(t)(YP.value)(DI)(r)}}},dI={q:1,gain:0},fl=function(t){return{toInitializePeaking:function(r){return Aa(t)(tI.value)(dI)(r)}}};var bI={q:1},ll=function(t){return{toInitializeNotch:function(r){return Aa(t)(rI.value)(bI)(r)}}};var yI={gain:0},Qh=function(t){return{toInitializeLowshelf:function(r){return Aa(t)(eI.value)(yI)(r)}}};var AI={q:1},vd=function(t){return{toInitializeLowpass:function(r){return Aa(t)(nI.value)(AI)(r)}}},om={toInitializeLowpass:function(){var t=fd(vd(Et(ht()(J(Ct)(nd)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},kI=function(){return{loopStart:0,loopEnd:0,playbackRate:1,duration:W.value}}(),uf=function(t){return{toInitializeLoopBuf:function(r){return Aa(t)(aI.value)(kI)(r)}}},Er={toInitializeLoopBuf:function(){var t=ld(uf(Et(ht()(J(Ct)(af)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())));return function(r){return t(function(e){return{buffer:e}}(r))}}()},gI={gain:0},Kh=function(t){return{toInitializeHighshelf:function(r){return Aa(t)(uI.value)(gI)(r)}}};var CI={q:1},Dd=function(t){return{toInitializeHighpass:function(r){return Aa(t)(oI.value)(CI)(r)}}},mu={toInitializeHighpass:function(){var t=_d(Dd(Et(ht()(J(Ct)(ad)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},hI=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),Yh=function(t){return{toInitializeDynamicsCompressor:function(r){return Aa(t)(iI.value)(hI)(r)}}},EI={maxDelayTime:1},dd=function(t){return{toInitializeDelay:function(r){return Aa(t)(cI.value)(EI)(r)}}},nn={toInitializeDelay:function(){var t=pd(dd(Et(ht()(J(Ct)(ud)()()()({reflectSymbol:function(){return"delayTime"}})))(gt()())));return function(r){return t(function(e){return{delayTime:e}}(r))}}()},TI={q:1},Dn=function(t){return{toInitializeBandpass:function(r){return Aa(t)(fI.value)(TI)(r)}}},bd={toInitializeBandpass:function(){var t=sd(Dn(Et(ht()(J(Ct)(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},SI={q:1},im=function(t){return{toInitializeAllpass:function(r){return Aa(t)(lI.value)(SI)(r)}}},yd={toInitializeAllpass:function(){var t=md(im(Et(ht()(J(Ct)(um)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()};var ni=function(){function t(){this.head=null,this.last=null,this.size=0}function r(f,m){this.queue=f,this.value=m,this.next=null,this.prev=null}function e(f){this.draining=!1,this.error=null,this.value=f,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function a(f){try{f()}catch(m){setTimeout(function(){throw m},0)}}function u(f,m){var v=new r(f,m);switch(f.size){case 0:f.head=v;break;case 1:v.prev=f.head,f.head.next=v,f.last=v;break;default:v.prev=f.last,f.last.next=v,f.last=v}return f.size++,v}function i(f){var m;switch(f.size){case 0:return null;case 1:m=f.head,f.head=null;break;case 2:m=f.last,f.head.next=null,f.last=null;break;default:m=f.last,f.last=m.prev,f.last.next=null}return m.prev=null,m.queue=null,f.size--,m.value}function o(f){var m;switch(f.size){case 0:return null;case 1:m=f.head,f.head=null;break;case 2:m=f.head,f.last.prev=null,f.head=f.last,f.last=null;break;default:m=f.head,f.head=m.next,f.head.prev=null}return m.next=null,m.queue=null,f.size--,m.value}function p(f){if(f.queue!==null){if(f.queue.last===f){i(f.queue);return}if(f.queue.head===f){o(f.queue);return}f.prev&&(f.prev.next=f.next),f.next&&(f.next.prev=f.prev),f.queue.size--,f.queue=null,f.value=null,f.next=null,f.prev=null}}function s(f,m){if(!m.draining){var v=m.puts,c=m.takes,h=m.reads,ut,dt,fr,Zt,Kr;for(m.draining=!0;;){if(ut=null,dt=null,fr=null,Zt=m.value,Kr=h.size,m.error!==null){for(Zt=f.left(m.error);ut=o(v);)a(ut.cb(Zt));for(;dt=o(h);)a(dt(Zt));for(;fr=o(c);)a(fr(Zt));break}if(Zt===n&&(ut=o(v))&&(m.value=Zt=ut.value),Zt!==n){for(fr=o(c);Kr--&&(dt=o(h));)a(dt(f.right(Zt)));fr!==null&&(m.value=n,a(fr(f.right(Zt))))}if(ut!==null&&a(ut.cb(f.right(void 0))),m.value===n&&v.size===0||m.value!==n&&c.size===0)break}m.draining=!1}}return e.EMPTY=n,e.putLast=u,e.takeLast=i,e.takeHead=o,e.deleteCell=p,e.drainVar=s,e}();function Q_(){return new ni(ni.EMPTY)}function tE(t,r,e){return function(){var n=ni.putLast(r.takes,e);return ni.drainVar(t,r),function(){ni.deleteCell(n)}}}function rE(t,r,e){return function(){return e.value===ni.EMPTY&&e.error===null?(e.value=r,ni.drainVar(t,e),!0):!1}}function eE(t,r){return function(){var e=r.value;return e===ni.EMPTY?t.nothing:(r.value=ni.EMPTY,ni.drainVar(t,r),t.just(e))}}var $I=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),wI=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),MI=function(){function t(){}return t.value=new t,t}();var Ad=function(){return{left:Kt.create,right:Yt.create,nothing:W.value,just:F.create,killed:$I.create,filled:wI.create,empty:MI.value}}();var nE=function(t){return function(r){return tE(Ad,t,r)}},cm=function(t){return function(r){return rE(Ad,t,r)}};var aE=function(t){return eE(Ad,t)};var PI=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var II=function(){function t(){}return t.value=new t,t}();var fm={convertOption:function(t){return function(r){return rt(et)}}},lm={convertOption:function(t){return function(r){return rt(et)}}};var RI=function(t){return t.toInitializeAnalyser},vu=zk(G)({doLogic:$c,ids:function(){var t=Ee();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}})({fromElt:Gn(),connectToParent:function(t){return function(r){return t.connectXToY({from:r.id,to:r.parent})}}});var NI=function(){return{cb:function(t){return l($)(l($)(void 0))},fftSize:QD.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:YD.value,channelInterpretation:KD.value}}(),_m=function(t){return{toInitializeAnalyser:function(r){return Aa(t)(II.value)(NI)(r)}}};var LI=function(t){return function(r){var e=Wh(t)(r),n=function(a){return function(u){return Ht(function(i){return function(){var p=u.ids();return a.raiseId(p)(),_(S)(function(s){return Q(ot)(i(u.deleteFromCache({id:p})))(s)})(Mt($t)(i)(l(C(G))(u.makeMicrophone({id:p,parent:a.parent,scope:a.scope,microphone:e.microphone}))))()}})}};return new M(n)}},K_=function(t){return LI(t)};var ea=Qu($)(G)({doLogic:$c,ids:function(){var t=Ee();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}}),BI=function(t){return function(r){return function(e){return function(n){var a=RI(t)(r),u=function(i){return function(o){return Ht(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(R(N($))(l(C(G))(o.makeAnalyser({id:f,parent:i.parent,scope:i.scope,cb:a.cb,fftSize:fD(2)(function(){if(a.fftSize instanceof Ks)return 7;if(a.fftSize instanceof Ys)return 8;if(a.fftSize instanceof zC)return 9;if(a.fftSize instanceof VC)return 10;if(a.fftSize instanceof QD)return 11;if(a.fftSize instanceof GC)return 12;if(a.fftSize instanceof JC)return 13;throw new Error("Failed pattern match at Ocarina.Control (line 189, column 21 - line 196, column 34): "+[a.fftSize.constructor.name])}()),maxDecibels:a.maxDecibels,minDecibels:a.minDecibels,smoothingTimeConstant:a.smoothingTimeConstant,channelCount:a.channelCount,channelCountMode:function(){if(a.channelCountMode instanceof ih)return"explicit";if(a.channelCountMode instanceof YD)return"max";if(a.channelCountMode instanceof oh)return"clamped-max";throw new Error("Failed pattern match at Ocarina.Control (line 202, column 35 - line 205, column 46): "+[a.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(a.channelInterpretation instanceof KD)return"speakers";if(a.channelInterpretation instanceof uh)return"discrete";throw new Error("Failed pattern match at Ocarina.Control (line 206, column 40 - line 208, column 41): "+[a.channelInterpretation.constructor.name])}()})))(R(N($))(_(g)(function(m){return Ve()()()({cb:function(v){return o.setAnalyserNodeCb({id:f,cb:v})}})(m)})(e))(ea({parent:new F(f),scope:i.scope,raiseId:Pt(tn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},pm=function(t){return function(r){return BI(t)(r)(O(E($)))}},oE=function(t){return function(r){return function(e){var n=Jh(t)(r),a=function(u){return function(i){return Ht(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(R(N($))(l(C(G))(i.makeConvolver({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer})))(ea({parent:new F(s),scope:u.scope,raiseId:Pt(tn(Le(Ne)))})(i)(U(e)))))()}})}};return new M(a)}}},HI=function(){return function(){return function(t){return function(r){return function(e){return function(n){return function(a){var u=Zh(t)(n)(r)(e),i=function(o){return function(p){return Ht(function(s){return function(){var m=p.ids();return o.raiseId(m)(),_(S)(function(v){return Q(ot)(s(p.deleteFromCache({id:m})))(v)})(Mt($t)(s)(R(N($))(l(C(G))(p.makeIIRFilter({id:m,parent:o.parent,scope:o.scope,feedforward:zc()(u.feedforward),feedback:zc()(u.feedback)})))(ea({parent:new F(m),scope:o.scope,raiseId:Pt(tn(Le(Ne)))})(p)(U(a)))))()}})}};return new M(i)}}}}}}},iE=function(){return function(){return function(t){return HI()()(t)(d.value)(d.value)}}},kd=function(t){return function(r){return function(e){var n=Lh(t)(r),a=function(u){return function(i){return Ht(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(R(N($))(l(C(G))(i.makeRecorder({id:s,parent:u.parent,scope:u.scope,cb:n.cb})))(ea({parent:new F(s),scope:u.scope,raiseId:Pt(tn(Le(Ne)))})(i)(e))))()}})}};return new M(a)}}},UI=function(t){return function(r){return Ht(function(e){return function(){var a=r.ids();return e(r.makeSpeaker({id:a}))(),$t(ea({parent:new F(a),scope:new Ei("toplevel"),raiseId:Pt(tn(Le(Ne)))})(r)(U(t)))(e)()}})}},of=UI,Nt=function(t){return function(r){return function(e){return Ge(t)(r)(O(E($)))(e)}}},Ge=function(t){return function(r){return function(e){return function(n){var a=Vh(t)(r),u=function(i){return function(o){return Ht(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(R(N($))(l(C(G))(o.makeGain({id:f,parent:i.parent,scope:i.scope,gain:a.gain})))(R(N($))(xe(Jt(G))(_(g)(function(m){return Ve()()()({gain:cE(591)(i.scope)(o)(function(v){return o.setGain(function(c){return{id:f,gain:c}}(v))})})(m)})(e)))(ea({parent:new F(f),scope:i.scope,raiseId:Pt(tn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},cE=PI("tmpResolveAU","Ocarina.Control",function(){var t=function(){var i=le()({reflectSymbol:function(){return"unit"}})(d.value);return function(o){return rl(i(o))}}(),r=function(){var i=le()({reflectSymbol:function(){return"sudden"}})(d.value);return function(o){return rl(i(o))}}(),e=function(){var i=le()({reflectSymbol:function(){return"numeric"}})(d.value);return function(o){return rl(i(o))}}(),n=function(){var i=le()({reflectSymbol:function(){return"envelope"}})(d.value);return function(o){return rl(i(o))}}(),a=function(){var i=le()({reflectSymbol:function(){return"cancel"}})(d.value);return function(o){return rl(i(o))}}(),u=function(i){return function(o){return function(p){return function(s){return Ve()()()({numeric:function(){var f=l(C(G));return function(m){return f(p(e(m)))}}(),envelope:function(){var f=l(C(G));return function(m){return f(p(n(m)))}}(),cancel:function(){var f=l(C(G));return function(m){return f(p(a(m)))}}(),sudden:function(){var f=l(C(G));return function(m){return f(p(r(m)))}}(),unit:function(f){var m=Nt(lt)(1)([f.u]);return Ht(function(v){return function(){var h=Q_();return $t(R(N($))(ea({parent:W.value,scope:i,raiseId:function(ut){return cr(S)(cm(ut)(h))}})(o)(m))(Ht(function(ut){return function(){return cr(S)(nE(h)(function(fr){if(fr instanceof Kt)return jc(fr.value0);if(fr instanceof Yt)return ut(p(t({i:fr.value0})));throw new Error("Failed pattern match at Ocarina.Control (line 1674, column 39 - line 1677, column 66): "+[fr.constructor.name])}))(),l($)(void 0)}})))(v)()}})}})(s)}}}};return u}),me=cE(1653),WI=function(t){return function(r){return function(e){var n=ld(t)(r),a=function(u){return function(i){return Ht(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(R(N($))(l(C(G))(i.makeLoopBuf({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer,playbackRate:n.playbackRate,loopStart:n.loopStart,loopEnd:n.loopEnd,duration:n.duration})))(xe(Jt(G))(_(g)(function(f){return Ve()()()({buffer:function(m){return l(C(G))(i.setBuffer({id:s,buffer:m}))},playbackRate:me(u.scope)(i)(function(m){return i.setPlaybackRate(function(v){return{id:s,playbackRate:v}}(m))}),loopStart:function(m){return l(C(G))(i.setLoopStart({id:s,loopStart:m}))},loopEnd:function(m){return l(C(G))(i.setLoopEnd({id:s,loopEnd:m}))},onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))}})(f)})(e)))))()}})}};return new M(a)}}},sr=function(t){return WI(t)};var qI=function(t){return function(r){return function(e){var n=Bh(t)(r),a=function(u){return function(i){return Ht(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(R(N($))(l(C(G))(i.makePeriodicOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency,spec:n.spec})))(xe(Jt(G))(_(g)(function(f){return Ve()()()({frequency:me(u.scope)(i)(function(m){return i.setFrequency(function(v){return{id:s,frequency:v}}(m))}),onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))},spec:function(m){return l(C(G))(i.setPeriodicOsc({id:s,spec:m}))}})(f)})(e)))))()}})}};return new M(a)}}},Ii=function(t){return qI(t)};var ZI=function(t){return function(r){return function(e){var n=cd(t)(r),a=function(u){return function(i){return Ht(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(R(N($))(l(C(G))(i.makePlayBuf({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer,playbackRate:n.playbackRate,bufferOffset:n.bufferOffset,duration:n.duration})))(xe(Jt(G))(_(g)(function(f){return Ve()()()({buffer:function(m){return l(C(G))(i.setBuffer({id:s,buffer:m}))},playbackRate:me(u.scope)(i)(function(m){return i.setPlaybackRate(function(v){return{id:s,playbackRate:v}}(m))}),bufferOffset:function(m){return l(C(G))(i.setBufferOffset({id:s,bufferOffset:m}))},onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))},duration:function(m){return l(C(G))(i.setDuration({id:s,duration:m}))}})(f)})(e)))))()}})}};return new M(a)}}},na=function(t){return ZI(t)};var zI=function(t){return function(r){return function(e){var n=Nh(t)(r),a=function(u){return function(i){return Ht(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(R(N($))(l(C(G))(i.makeSawtoothOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(xe(Jt(G))(_(g)(function(f){return Ve()()()({frequency:me(u.scope)(i)(function(m){return i.setFrequency(function(v){return{id:s,frequency:v}}(m))}),onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))}})(f)})(e)))))()}})}};return new M(a)}}},fE=function(t){return zI(t)};var VI=function(t){return function(r){return function(e){var n=Rh(t)(r),a=function(u){return function(i){return Ht(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(R(N($))(l(C(G))(i.makeSinOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(xe(Jt(G))(_(g)(function(f){return Ve()()()({frequency:me(u.scope)(i)(function(m){return i.setFrequency(function(v){return{id:s,frequency:v}}(m))}),onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))}})(f)})(e)))))()}})}};return new M(a)}}},cf=function(t){return VI(t)},lE=function(t){return function(r){return cf(t)(r)(O(E($)))}},GI=function(t){return function(r){return function(e){var n=Ih(t)(r),a=function(u){return function(i){return Ht(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(R(N($))(l(C(G))(i.makeSquareOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(xe(Jt(G))(_(g)(function(f){return Ve()()()({frequency:me(u.scope)(i)(function(m){return i.setFrequency(function(v){return{id:s,frequency:v}}(m))}),onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))}})(f)})(e)))))()}})}};return new M(a)}}},Y_=function(t){return GI(t)},_E=function(t){return function(r){return Y_(t)(r)(O(E($)))}},JI=function(t){return function(r){return function(e){var n=Mh(t)(r),a=function(u){return function(i){return Ht(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(R(N($))(l(C(G))(i.makeTriangleOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(xe(Jt(G))(_(g)(function(f){return Ve()()()({frequency:me(u.scope)(i)(function(m){return i.setFrequency(function(v){return{id:s,frequency:v}}(m))}),onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))}})(f)})(e)))))()}})}};return new M(a)}}},sm=function(t){return JI(t)};var jI=function(t){return function(r){return function(e){return function(n){var a=md(t)(r),u=function(i){return function(o){return Ht(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(R(N($))(l(C(G))(o.makeAllpass({id:f,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(R(N($))(xe(Jt(G))(_(g)(function(m){return Ve()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),q:me(i.scope)(o)(function(v){return o.setQ(function(c){return{id:f,q:c}}(v))})})(m)})(e)))(ea({parent:new F(f),scope:i.scope,raiseId:Pt(tn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},tp=function(t){return function(r){return function(e){return jI(t)(r)(O(E($)))(e)}}},gd=function(t){return function(r){return function(e){return function(n){var a=sd(t)(r),u=function(i){return function(o){return Ht(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(R(N($))(l(C(G))(o.makeBandpass({id:f,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(R(N($))(xe(Jt(G))(_(g)(function(m){return Ve()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),q:me(i.scope)(o)(function(v){return o.setQ(function(c){return{id:f,q:c}}(v))})})(m)})(e)))(ea({parent:new F(f),scope:i.scope,raiseId:Pt(tn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},$n=function(t){return function(r){return function(e){return gd(t)(r)(O(E($)))(e)}}},rp=function(t){return function(r){return function(e){return function(n){var a=pd(t)(r),u=function(i){return function(o){return Ht(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(R(N($))(l(C(G))(o.makeDelay({id:f,parent:i.parent,scope:i.scope,delayTime:a.delayTime,maxDelayTime:a.maxDelayTime})))(R(N($))(xe(Jt(G))(_(g)(function(m){return Ve()()()({delayTime:me(i.scope)(o)(function(v){return o.setDelay(function(c){return{id:f,delayTime:c}}(v))})})(m)})(e)))(ea({parent:new F(f),scope:i.scope,raiseId:Pt(tn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},Po=function(t){return function(r){return function(e){return rp(t)(r)(O(E($)))(e)}}},XI=function(t){return function(r){return function(e){return function(n){var a=Gh(t)(r),u=function(i){return function(o){return Ht(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(R(N($))(l(C(G))(o.makeDynamicsCompressor({id:f,parent:i.parent,scope:i.scope,threshold:a.threshold,ratio:a.ratio,knee:a.knee,attack:a.attack,release:a.release})))(R(N($))(xe(Jt(G))(_(g)(function(m){return Ve()()()({threshold:me(i.scope)(o)(function(v){return o.setThreshold(function(c){return{id:f,threshold:c}}(v))}),ratio:me(i.scope)(o)(function(v){return o.setRatio(function(c){return{id:f,ratio:c}}(v))}),knee:me(i.scope)(o)(function(v){return o.setKnee(function(c){return{id:f,knee:c}}(v))}),attack:me(i.scope)(o)(function(v){return o.setAttack(function(c){return{id:f,attack:c}}(v))}),release:me(i.scope)(o)(function(v){return o.setRelease(function(c){return{id:f,release:c}}(v))})})(m)})(e)))(ea({parent:new F(f),scope:i.scope,raiseId:Pt(tn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},pE=function(t){return function(r){return XI(t)(r)(O(E($)))}},QI=function(){return function(t){return function(r){return tD()(G)({doLogic:$c,ids:function(){var e=Ee();return function(n){return function(a){return a.ids}(e(n))}}(),disconnectElement:function(e){return function(n){return e.disconnectXFromY({from:n.id,to:n.parent})}},toElt:function(e){return e}})({fromEltO1:Gn(),fromEltO2:Gn(),toElt:Gn(),wrapElt:function(e){return Nt(lt)(1)([e])},giveNewParent:function(e){return function(n){return function(a){return e.connectXToY({from:n.id,to:n.parent})}}},deleteFromCache:function(){var e=Ee();return function(n){return function(a){return a.deleteFromCache}(e(n))}}()})(t)(Ra(pn)(_(d_)(function(e){return e(void 0)}))(Gn()(r)))}}},Ba=function(t){return function(r){return QI()(wk(t))(Ra(pn)(Mk()()()()()({reflectType:function(){return 0}})(d.value))(r))}};var Cd=function(t){return function(r){return function(e){return function(n){var a=_d(t)(r),u=function(i){return function(o){return Ht(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(R(N($))(l(C(G))(o.makeHighpass({id:f,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(R(N($))(xe(Jt(G))(_(g)(function(m){return Ve()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),q:me(i.scope)(o)(function(v){return o.setQ(function(c){return{id:f,q:c}}(v))})})(m)})(e)))(ea({parent:new F(f),scope:i.scope,raiseId:Pt(tn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},_l=function(t){return function(r){return function(e){return Cd(t)(r)(O(E($)))(e)}}},KI=function(t){return function(r){return function(e){return function(n){var a=zh(t)(r),u=function(i){return function(o){return Ht(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(R(N($))(l(C(G))(o.makeHighshelf({id:f,parent:i.parent,scope:i.scope,frequency:a.frequency,gain:a.gain})))(R(N($))(xe(Jt(G))(_(g)(function(m){return Ve()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),gain:me(i.scope)(o)(function(v){return o.setGain(function(c){return{id:f,gain:c}}(v))})})(m)})(e)))(ea({parent:new F(f),scope:i.scope,raiseId:Pt(tn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},sE=function(t){return function(r){return function(e){return KI(t)(r)(O(E($)))(e)}}},mE=function(t){return function(r){return function(e){return function(n){var a=fd(t)(r),u=function(i){return function(o){return Ht(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(R(N($))(l(C(G))(o.makeLowpass({id:f,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(R(N($))(xe(Jt(G))(_(g)(function(m){return Ve()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),q:me(i.scope)(o)(function(v){return o.setQ(function(c){return{id:f,q:c}}(v))})})(m)})(e)))(ea({parent:new F(f),scope:i.scope,raiseId:Pt(tn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},pl=function(t){return function(r){return function(e){return mE(t)(r)(O(E($)))(e)}}},YI=function(t){return function(r){return function(e){return function(n){var a=qh(t)(r),u=function(i){return function(o){return Ht(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(R(N($))(l(C(G))(o.makeLowshelf({id:f,parent:i.parent,scope:i.scope,frequency:a.frequency,gain:a.gain})))(R(N($))(xe(Jt(G))(_(g)(function(m){return Ve()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),gain:me(i.scope)(o)(function(v){return o.setGain(function(c){return{id:f,gain:c}}(v))})})(m)})(e)))(ea({parent:new F(f),scope:i.scope,raiseId:Pt(tn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},vE=function(t){return function(r){return function(e){return YI(t)(r)(O(E($)))(e)}}},tR=function(t){return function(r){return function(e){return function(n){var a=Uh(t)(r),u=function(i){return function(o){return Ht(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(R(N($))(l(C(G))(o.makeNotch({id:f,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q})))(R(N($))(xe(Jt(G))(_(g)(function(m){return Ve()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),q:me(i.scope)(o)(function(v){return o.setQ(function(c){return{id:f,q:c}}(v))})})(m)})(e)))(ea({parent:new F(f),scope:i.scope,raiseId:Pt(tn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},sl=function(t){return function(r){return function(e){return tR(t)(r)(O(E($)))(e)}}},rR=function(t){return function(r){return function(e){return function(n){var a=Ph(t)(r),u=function(i){return function(o){return Ht(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(R(N($))(l(C(G))(o.makeStereoPanner({id:f,parent:i.parent,scope:i.scope,pan:a.pan})))(R(N($))(xe(Jt(G))(_(g)(function(m){return Ve()()()({pan:me(i.scope)(o)(function(v){return o.setPan(function(c){return{id:f,pan:c}}(v))})})(m)})(e)))(ea({parent:new F(f),scope:i.scope,raiseId:Pt(tn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},DE=function(t){return function(r){return rR(t)(r)(O(E($)))}},eR=function(t){return function(r){return function(e){return function(n){var a=Hh(t)(r),u=function(i){return function(o){return Ht(function(p){return function(){var f=o.ids();return i.raiseId(f)(),_(S)(function(m){return Q(ot)(p(o.deleteFromCache({id:f})))(m)})(Mt($t)(p)(R(N($))(l(C(G))(o.makePeaking({id:f,parent:i.parent,scope:i.scope,frequency:a.frequency,q:a.q,gain:a.gain})))(R(N($))(xe(Jt(G))(_(g)(function(m){return Ve()()()({frequency:me(i.scope)(o)(function(v){return o.setFrequency(function(c){return{id:f,frequency:c}}(v))}),q:me(i.scope)(o)(function(v){return o.setQ(function(c){return{id:f,q:c}}(v))}),gain:me(i.scope)(o)(function(v){return o.setGain(function(c){return{id:f,gain:c}}(v))})})(m)})(e)))(ea({parent:new F(f),scope:i.scope,raiseId:Pt(tn(Le(Ne)))})(o)(U(n))))))()}})}};return new M(u)}}}},ml=function(t){return function(r){return function(e){return eR(t)(r)(O(E($)))(e)}}},dE=function(t){return function(r){return function(e){var n=id(t)(r),a=function(u){return function(i){return Ht(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(R(N($))(l(C(G))(i.makeWaveShaper({id:s,parent:u.parent,scope:u.scope,curve:n.curve,oversample:n.oversample})))(ea({parent:new F(s),scope:u.scope,raiseId:Pt(tn(Le(Ne)))})(i)(U(e)))))()}})}};return new M(a)}}},nR=function(t){return function(r){return function(e){var n=jh(t)(r),a=function(u){return function(i){return Ht(function(o){return function(){var s=i.ids();return u.raiseId(s)(),_(S)(function(f){return Q(ot)(o(i.deleteFromCache({id:s})))(f)})(Mt($t)(o)(R(N($))(l(C(G))(i.makeConstant({id:s,parent:u.parent,scope:u.scope,offset:n.offset})))(xe(Jt(G))(_(g)(function(f){return Ve()()()({offset:me(u.scope)(i)(function(m){return i.setOffset(function(v){return{id:s,offset:v}}(m))}),onOff:function(m){return l(C(G))(i.setOnOff({id:s,onOff:m}))}})(f)})(e)))))()}})}};return new M(a)}}},mm=function(t){return nR(t)};function hd(){window.scrollTo(0,0)}var Io=function(t){return t.sequential},Hn=function(t){return t.parallel};var wn=function(t){return function(r){return new M(X("button")(t)(U(r)))}};var Ha=function(){var t={},r="Pure",e="Throw",n="Catch",a="Sync",u="Async",i="Bind",o="Bracket",p="Fork",s="Sequential",f="Map",m="Apply",v="Alt",c="Cons",h="Resume",ut="Release",dt="Finalizer",fr="Finalized",Zt="Forked",Kr="Fiber",re="Thunk";function pt(Vt,ee,je,ve){this.tag=Vt,this._1=ee,this._2=je,this._3=ve}function mr(Vt){var ee=function(je,ve,lr){return new pt(Vt,je,ve,lr)};return ee.tag=Vt,ee}function Fr(Vt){return new pt(r,void 0)}function ir(Vt){try{Vt()}catch(ee){setTimeout(function(){throw ee},0)}}function Je(Vt,ee,je){try{return ee(je())}catch(ve){return Vt(ve)}}function Ho(Vt,ee,je){try{return ee(je)()}catch(ve){return je(Vt(ve))(),Fr}}var io=function(){var Vt=1024,ee=0,je=0,ve=new Array(Vt),lr=!1;function xt(){var ue;for(lr=!0;ee!==0;)ee--,ue=ve[je],ve[je]=void 0,je=(je+1)%Vt,ue();lr=!1}return{isDraining:function(){return lr},enqueue:function(ue){var Ir,qe;ee===Vt&&(qe=lr,xt(),lr=qe),ve[(je+ee)%Vt]=ue,ee++,lr||xt()}}}();function Vi(Vt){var ee={},je=0,ve=0;return{register:function(lr){var xt=je++;lr.onComplete({rethrow:!0,handler:function(ue){return function(){ve--,delete ee[xt]}}})(),ee[xt]=lr,ve++},isEmpty:function(){return ve===0},killAll:function(lr,xt){return function(){if(ve===0)return xt();var ue=0,Ir={};function qe(ke){Ir[ke]=ee[ke].kill(lr,function(an){return function(){delete Ir[ke],ue--,Vt.isLeft(an)&&Vt.fromLeft(an)&&setTimeout(function(){throw Vt.fromLeft(an)},0),ue===0&&xt()}})()}for(var aa in ee)ee.hasOwnProperty(aa)&&(ue++,qe(aa));return ee={},je=0,ve=0,function(ke){return new pt(a,function(){for(var an in Ir)Ir.hasOwnProperty(an)&&Ir[an]()})}}}}}var Cu=0,bn=1,li=2,sf=3,mf=4,Pe=5,_i=6;function vf(Vt,ee,je){var ve=0,lr=Cu,xt=je,ue=null,Ir=null,qe=null,aa=null,ke=null,an=0,Oc=0,Bu=null,Gi=!0;function Ji(_r){for(var vr,zr,Yr;;)switch(vr=null,zr=null,Yr=null,lr){case li:lr=bn;try{xt=qe(xt),aa===null?qe=null:(qe=aa._1,aa=aa._2)}catch(Da){lr=Pe,ue=Vt.left(Da),xt=null}break;case sf:Vt.isLeft(xt)?(lr=Pe,ue=xt,xt=null):qe===null?lr=Pe:(lr=li,xt=Vt.fromRight(xt));break;case bn:switch(xt.tag){case i:qe&&(aa=new pt(c,qe,aa)),qe=xt._2,lr=bn,xt=xt._1;break;case r:qe===null?(lr=Pe,xt=Vt.right(xt._1)):(lr=li,xt=xt._1);break;case a:lr=sf,xt=Je(Vt.left,Vt.right,xt._1);break;case u:lr=mf,xt=Ho(Vt.left,xt._1,function(Da){return function(){ve===_r&&(ve++,io.enqueue(function(){ve===_r+1&&(lr=sf,xt=Da,Ji(ve))}))}});return;case e:lr=Pe,ue=Vt.left(xt._1),xt=null;break;case n:qe===null?ke=new pt(c,xt,ke,Ir):ke=new pt(c,xt,new pt(c,new pt(h,qe,aa),ke,Ir),Ir),qe=null,aa=null,lr=bn,xt=xt._1;break;case o:an++,qe===null?ke=new pt(c,xt,ke,Ir):ke=new pt(c,xt,new pt(c,new pt(h,qe,aa),ke,Ir),Ir),qe=null,aa=null,lr=bn,xt=xt._1;break;case p:lr=sf,vr=vf(Vt,ee,xt._2),ee&&ee.register(vr),xt._1&&vr.run(),xt=Vt.right(vr);break;case s:lr=bn,xt=aS(Vt,ee,xt._1);break}break;case Pe:if(qe=null,aa=null,ke===null)lr=_i,xt=Ir||ue||xt;else switch(vr=ke._3,Yr=ke._1,ke=ke._2,Yr.tag){case n:Ir&&Ir!==vr&&an===0?lr=Pe:ue&&(lr=bn,xt=Yr._2(Vt.fromLeft(ue)),ue=null);break;case h:Ir&&Ir!==vr&&an===0||ue?lr=Pe:(qe=Yr._1,aa=Yr._2,lr=li,xt=Vt.fromRight(xt));break;case o:an--,ue===null&&(zr=Vt.fromRight(xt),ke=new pt(c,new pt(ut,Yr._2,zr),ke,vr),(Ir===vr||an>0)&&(lr=bn,xt=Yr._3(zr)));break;case ut:ke=new pt(c,new pt(fr,xt,ue),ke,Ir),lr=bn,Ir&&Ir!==vr&&an===0?xt=Yr._1.killed(Vt.fromLeft(Ir))(Yr._2):ue?xt=Yr._1.failed(Vt.fromLeft(ue))(Yr._2):xt=Yr._1.completed(Vt.fromRight(xt))(Yr._2),ue=null,an++;break;case dt:an++,ke=new pt(c,new pt(fr,xt,ue),ke,Ir),lr=bn,xt=Yr._1;break;case fr:an--,lr=Pe,xt=Yr._1,ue=Yr._2;break}break;case _i:for(var Qe in Bu)Bu.hasOwnProperty(Qe)&&(Gi=Gi&&Bu[Qe].rethrow,ir(Bu[Qe].handler(xt)));Bu=null,Ir&&ue?setTimeout(function(){throw Vt.fromLeft(ue)},0):Vt.isLeft(xt)&&Gi&&setTimeout(function(){if(Gi)throw Vt.fromLeft(xt)},0);return;case Cu:lr=bn;break;case mf:return}}function Xe(_r){return function(){if(lr===_i)return Gi=Gi&&_r.rethrow,_r.handler(xt)(),function(){};var vr=Oc++;return Bu=Bu||{},Bu[vr]=_r,function(){Bu!==null&&delete Bu[vr]}}}function yr(_r,vr){return function(){if(lr===_i)return vr(Vt.right(void 0))(),function(){};var zr=Xe({rethrow:!1,handler:function(){return vr(Vt.right(void 0))}})();switch(lr){case Cu:Ir=Vt.left(_r),lr=_i,xt=Ir,Ji(ve);break;case mf:Ir===null&&(Ir=Vt.left(_r)),an===0&&(lr===mf&&(ke=new pt(c,new pt(dt,xt(_r)),ke,Ir)),lr=Pe,xt=null,ue=null,Ji(++ve));break;default:Ir===null&&(Ir=Vt.left(_r)),an===0&&(lr=Pe,xt=null,ue=null)}return zr}}function Lr(_r){return function(){var vr=Xe({rethrow:!1,handler:_r})();return lr===Cu&&Ji(ve),vr}}return{kill:yr,join:Lr,onComplete:Xe,isSuspended:function(){return lr===Cu},run:function(){lr===Cu&&(io.isDraining()?Ji(ve):io.enqueue(function(){Ji(ve)}))}}}function pi(Vt,ee,je,ve){var lr=0,xt={},ue=0,Ir={},qe=new Error("[ParAff] Early exit"),aa=null,ke=t;function an(Xe,yr,Lr){var _r=yr,vr=null,zr=null,Yr=0,Qe={},Da,Ml;t:for(;;)switch(Da=null,_r.tag){case Zt:if(_r._3===t&&(Da=xt[_r._1],Qe[Yr++]=Da.kill(Xe,function(uS){return function(){Yr--,Yr===0&&Lr(uS)()}})),vr===null)break t;_r=vr._2,zr===null?vr=null:(vr=zr._1,zr=zr._2);break;case f:_r=_r._2;break;case m:case v:vr&&(zr=new pt(c,vr,zr)),vr=_r,_r=_r._1;break}if(Yr===0)Lr(Vt.right(void 0))();else for(Ml=0,Da=Yr;Ml<Da;Ml++)Qe[Ml]=Qe[Ml]();return Qe}function Oc(Xe,yr,Lr){var _r,vr,zr,Yr,Qe,Da;Vt.isLeft(Xe)?(_r=Xe,vr=null):(vr=Xe,_r=null);t:for(;;){if(zr=null,Yr=null,Qe=null,Da=null,aa!==null)return;if(yr===null){ve(_r||vr)();return}if(yr._3!==t)return;switch(yr.tag){case f:_r===null?(yr._3=Vt.right(yr._1(Vt.fromRight(vr))),vr=yr._3):yr._3=_r;break;case m:if(zr=yr._1._3,Yr=yr._2._3,_r){if(yr._3=_r,Qe=!0,Da=ue++,Ir[Da]=an(qe,_r===zr?yr._2:yr._1,function(){return function(){delete Ir[Da],Qe?Qe=!1:Lr===null?Oc(_r,null,null):Oc(_r,Lr._1,Lr._2)}}),Qe){Qe=!1;return}}else{if(zr===t||Yr===t)return;vr=Vt.right(Vt.fromRight(zr)(Vt.fromRight(Yr))),yr._3=vr}break;case v:if(zr=yr._1._3,Yr=yr._2._3,zr===t&&Vt.isLeft(Yr)||Yr===t&&Vt.isLeft(zr))return;if(zr!==t&&Vt.isLeft(zr)&&Yr!==t&&Vt.isLeft(Yr))_r=vr===zr?Yr:zr,vr=null,yr._3=_r;else if(yr._3=vr,Qe=!0,Da=ue++,Ir[Da]=an(qe,vr===zr?yr._2:yr._1,function(){return function(){delete Ir[Da],Qe?Qe=!1:Lr===null?Oc(vr,null,null):Oc(vr,Lr._1,Lr._2)}}),Qe){Qe=!1;return}break}Lr===null?yr=null:(yr=Lr._1,Lr=Lr._2)}}function Bu(Xe){return function(yr){return function(){delete xt[Xe._1],Xe._3=yr,Oc(yr,Xe._2._1,Xe._2._2)}}}function Gi(){var Xe=bn,yr=je,Lr=null,_r=null,vr,zr;t:for(;;)switch(vr=null,zr=null,Xe){case bn:switch(yr.tag){case f:Lr&&(_r=new pt(c,Lr,_r)),Lr=new pt(f,yr._1,t,t),yr=yr._2;break;case m:Lr&&(_r=new pt(c,Lr,_r)),Lr=new pt(m,t,yr._2,t),yr=yr._1;break;case v:Lr&&(_r=new pt(c,Lr,_r)),Lr=new pt(v,t,yr._2,t),yr=yr._1;break;default:zr=lr++,Xe=Pe,vr=yr,yr=new pt(Zt,zr,new pt(c,Lr,_r),t),vr=vf(Vt,ee,vr),vr.onComplete({rethrow:!1,handler:Bu(yr)})(),xt[zr]=vr,ee&&ee.register(vr)}break;case Pe:if(Lr===null)break t;Lr._1===t?(Lr._1=yr,Xe=bn,yr=Lr._2,Lr._2=t):(Lr._2=yr,yr=Lr,_r===null?Lr=null:(Lr=_r._1,_r=_r._2))}for(ke=yr,zr=0;zr<lr;zr++)xt[zr].run()}function Ji(Xe,yr){aa=Vt.left(Xe);var Lr;for(var _r in Ir)if(Ir.hasOwnProperty(_r)){Lr=Ir[_r];for(_r in Lr)Lr.hasOwnProperty(_r)&&Lr[_r]()}Ir=null;var vr=an(Xe,ke,yr);return function(zr){return new pt(u,function(Yr){return function(){for(var Qe in vr)vr.hasOwnProperty(Qe)&&vr[Qe]();return Fr}})}}return Gi(),function(Xe){return new pt(u,function(yr){return function(){return Ji(Xe,yr)}})}}function aS(Vt,ee,je){return new pt(u,function(ve){return function(){return pi(Vt,ee,je,ve)}})}return pt.EMPTY=t,pt.Pure=mr(r),pt.Throw=mr(e),pt.Catch=mr(n),pt.Sync=mr(a),pt.Async=mr(u),pt.Bind=mr(i),pt.Bracket=mr(o),pt.Fork=mr(p),pt.Seq=mr(s),pt.ParMap=mr(f),pt.ParApply=mr(m),pt.ParAlt=mr(v),pt.Fiber=vf,pt.Supervisor=Vi,pt.Scheduler=io,pt.nonCanceler=Fr,pt}(),bE=Ha.Pure,_R=Ha.Throw;function yE(t){return function(r){return r.tag===Ha.Pure.tag?Ha.Pure(t(r._1)):Ha.Bind(r,function(e){return Ha.Pure(t(e))})}}function AE(t){return function(r){return Ha.Bind(t,r)}}var kE=Ha.Sync;function gE(t){return function(r){return Ha.ParMap(t,r)}}function CE(t){return function(r){return Ha.ParApply(t,r)}}function hE(t){return function(r){return Ha.ParAlt(t,r)}}var vl=Ha.Async;function EE(t,r){return function(){return Ha.Fiber(t,null,r)}}var pR=function(){function t(e,n){return e===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,e)}function r(e,n){return e===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(e,n){return Ha.Async(function(a){return function(){var u=t(n,a(e()));return function(){return Ha.Sync(function(){return e(r(n,u))})}}})}}(),TE=Ha.Seq;var mR=function(t){return function(r){return function(e){var n=Io(t),a=ce(t.Applicative1())(r)(function(){var u=Hn(t);return function(i){return u(e(i))}}());return function(u){return n(a(u))}}}},SE=function(t){return function(r){return function(e){var n=Io(t),a=Rn(r)(t.Applicative1())(function(){var u=Hn(t);return function(i){return u(e(i))}}());return function(u){return n(a(u))}}}},xE=function(t){return function(r){return mR(t)(r)(rt(et))}};var vR=function(t){return t};var OE=function(t){return t};var np=function(t){return t.toDuration};var $E={fromDuration:cv()()(vR)(function(t){return t*1e3}),toDuration:cv()()(OE)(function(t){return t/1e3})};var wE=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var dR=function(t){return t};var dl={map:gE},Ri={map:yE};var bR=function(){var t=function(n){if(n instanceof Yt)return n.value0;if(n instanceof Kt)return lu("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): "+[n.constructor.name])},r=function(n){if(n instanceof Kt)return n.value0;if(n instanceof Yt)return lu("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): "+[n.constructor.name])},e=function(n){if(n instanceof Kt)return!0;if(n instanceof Yt)return!1;throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): "+[n.constructor.name])};return{isLeft:e,fromLeft:r,fromRight:t,left:Kt.create,right:Yt.create}}(),yR=function(t){return EE(bR,t)},Ro=function(t){return function(){var e=yR(t)();return e.run(),e}},ui=function(){var t=cr(S);return function(r){return t(Ro(r))}}();var Ni={apply:CE,Functor0:function(){return dl}};var Ed={Applicative0:function(){return Ta},Bind1:function(){return He}},He={bind:AE,Apply0:function(){return Td(0)}},Ta={pure:bE,Apply0:function(){return Td(0)}},Td=wE("applyAff","Effect.Aff",function(){return{apply:lo(Ed),Functor0:function(){return Ri}}}),ME=Td(71);var Be={liftEffect:kE,Monad0:function(){return Ed}},PE=function(){var t=Ae(Be);return function(r){return dR(T(t(r)))}}(),IE=function(t){return vl(function(r){return _(S)(PE)(t.join(r))})};var RE=function(t){return function(r){return P(He)(Ae(Be)(r.isSuspended))(function(e){return e?Ae(Be)(cr(S)(r.kill(t,T(l($)(void 0))))):vl(function(n){return _(S)(PE)(r.kill(t,n))})})}};var Un={parallel:mt,sequential:TE,Monad0:function(){return Ed},Applicative1:function(){return AR(0)}},AR=wE("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=Hn(Un),r=l(Ta);return function(e){return t(r(e))}}(),Apply0:function(){return Ni}}});var kR={append:function(t){return function(r){return function(e){return xE(Un)(Bt)([t(e),r(e)])}}}};var gR=T(l(Ta)(void 0)),NE={mempty:gR,Semigroup0:function(){return kR}};var LE={alt:hE,Functor0:function(){return dl}};var BE=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),vm=function(){function t(){}return t.value=new t,t}(),ff=function(){function t(){}return t.value=new t,t}(),ap=function(){function t(){}return t.value=new t,t}(),lf=function(){function t(){}return t.value=new t,t}(),up=function(){function t(){}return t.value=new t,t}(),op=function(){function t(){}return t.value=new t,t}(),HE=function(){function t(){}return t.value=new t,t}(),Dm=function(){function t(){}return t.value=new t,t}(),dm=function(){function t(){}return t.value=new t,t}(),ip=function(){function t(){}return t.value=new t,t}(),cp=function(){function t(){}return t.value=new t,t}(),UE=function(){function t(){}return t.value=new t,t}(),fp=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Sd=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var CR="numeric",hR="sudden",ER="unit",TR="cancel",SR="step",xR="linear",FR="exponential",OR="envelope",WE=function(t,r,e,n){if(e.type===hR)t.value=e.value.n;else if(e.type===ER)r.id&&wR(r.id,n),n.units[e.value.i].main.connect(t),r.id=e.value.i;else if(e.type===CR)t[e.value.t.type===SR?"setValueAtTime":e.value.t.type===xR?"linearRampToValueAtTime":e.value.t.type===FR?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](e.value.n,e.value.o);else if(e.type===TR)e.value.hold?t.cancelAndHoldAtTime(e.value.o):t.cancelScheduledValues(e.value.o);else if(e.type===OR){let a=e.value.o;t.cancelScheduledValues(Math.max(0,a)),t.setValueCurveAtTime(e.value.p,a,e.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(e))},$R=function(t,r,e,n,a){return n[e]||(n[e]={}),WE(r.parameters.get(e),n[e],a,t)},ao=function(t,r,e,n,a){return n[e]||(n[e]={}),WE(r[e],n[e],a,t)},Fe=function(t,r,e){let n=r.value0?r.value0:"@fan@";e.scopes[n]||(e.scopes[n]=[]),e.scopes[n].push(t),e.units[t].scope=n},Oe=function(t,r){r.toConnect[t]&&(r.toConnect[t].forEach(function(e){e.w?r.units[e.w]?e.f():(r.toConnect[e.w]||(r.toConnect[e.w]=[]),r.toConnect[e.w].push({f:e.f})):e.f()}),delete r.toConnect[t])},$e=function(t,r,e,n){t()(a=>qE(r,a,n))(e)},qE=function(t,r,e){var n=function(){e.units[t].audioOutgoing.push(r),e.units[t].pendingOn||(e.units[t].main.connect(e.units[r].main),e.units[r].se&&e.units[t].main.connect(e.units[r].se))};if(!e.units[t]){e.toConnect[t]||(e.toConnect[t]=[]);var a={f:n};r!==t&&!e.units[r]&&(a.w=r),e.toConnect[t].push(a);return}if(!e.units[r]){e.toConnect[r]||(e.toConnect[r]=[]);var a={f:n};r!==t&&!e.units[t]&&(a.w=t),e.toConnect[r].push(a);return}n()};function xd(t){return function(r){return function(){delete r.units[t.id]}}}function Fd(t){return function(r){return function(){qE(t.from,t.to,r)}}}var wR=function(t,r){if(r.units[t].scope==="@fan@")return;let e=r.units[t].scope;r.scopes[e].forEach(n=>{delete r.units[n]}),delete r.scopes[e]};function Od(t){return function(r){return function(){var e=t.from,n=t.to;if(!r.units[e]||(r.units[e].audioOutgoing=r.units[e].audioOutgoing.filter(function(u){return u!==n}),r.units[e].main.disconnect(r.units[n].main),r.units[n].se&&r.units[e].main.disconnect(r.units[n].se),r.units[e].scope==="@fan@"))return;let a=r.units[e].scope;r.scopes[a].forEach(u=>{delete r.units[u]}),delete r.scopes[a]}}}var $d=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"allpass",Q:r.q,frequency:r.frequency})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},wd=t=>r=>e=>()=>{var n=r.id,a=r.cb,u=new AnalyserNode(e.context,r),i=a(u)();e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:a,analyser:i,main:e.context.createGain(),se:u},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Md=t=>r=>e=>()=>{var n=r.id,a=r.options;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(e.context,a.name,{numberOfInputs:a.numberOfInputs,numberOfOutputs:a.numberOfOutputs,outputChannelCount:a.outputChannelCount,parameterData:a.parameterData,processorOptions:a.processorOptions})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Pd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"bandpass",Q:r.q,frequency:r.frequency})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Id=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new ConstantSourceNode(i,o)},u={offset:r.offset};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Rd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(e.context,{buffer:r.buffer})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Nd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(e.context,{delayTime:r.delayTime,maxDelayTime:r.maxDelayTime})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Ld=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(e.context,{knee:r.knee,ratio:r.ratio,threshold:r.threshold,attack:r.attack,release:r.release})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Bd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(e.context,{gain:r.gain})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Hd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highpass",Q:r.q,frequency:r.frequency})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Ud=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highshelf",frequency:r.frequency,gain:r.gain})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Wd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(e.context,{feedforward:r.feedforward,feedback:r.feedback})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},qd=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new AudioBufferSourceNode(i,o)},u={loop:!0,buffer:r.buffer,loopStart:r.loopStart,loopEnd:r.loopEnd,playbackRate:r.playbackRate};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Zd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowpass",Q:r.q,frequency:r.frequency})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},zd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowshelf",frequency:r.frequency,gain:r.gain})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Vd=t=>r=>e=>()=>{var n=r.id,a=r.element,u=function(){var i=e.context.createMediaElementSource(a);return i};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:u,resumeClosure:{},main:u()},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Gd=t=>r=>e=>()=>{var n=r.id;e.units[r.id]={main:e.context.createMediaStreamSource(r.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Jd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"notch",frequency:r.frequency,Q:r.q})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},jd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"peaking",frequency:r.frequency,Q:r.q,gain:r.gain})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Xd=t=>r=>e=>()=>{var n=r.id,a=function(i,o){var p={frequency:o.frequency,periodicWave:o.spec.type==="wave"?o.spec.value:Pb(e.context)(o.spec.value.real)(o.spec.value.img)()},s=new OscillatorNode(i,p);return s},u={frequency:r.frequency,type:"custom",spec:r.spec};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Qd=t=>r=>e=>()=>{var n=r.id,a=function(i,o){var p={loop:o.loop,buffer:o.buffer,playbackRate:o.playbackRate};return new AudioBufferSourceNode(i,p)},u={loop:!1,buffer:r.buffer,playbackRate:r.playbackRate,bufferOffset:r.bufferOffset,duration:t(void 0)(i=>i)(r.duration)};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Kd=t=>r=>e=>()=>{var n=r.id,a=r.cb,u=e.context.createMediaStreamDestination(),i=new MediaRecorder(u.stream);a(i)(),i.start(),e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:a,recorder:i,main:e.context.createGain(),se:u},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},Yd=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"sawtooth"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},tb=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"sine"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},rb=t=>r=>()=>{r.units[t.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:r.context.createGain(),se:r.context.destination}},eb=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(e.context,{pan:r.pan})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},nb=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"square"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},ab=t=>r=>e=>()=>{var n=r.id,a=function(i,o){return new OscillatorNode(i,o)},u={frequency:r.frequency,type:"triangle"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)},ub=t=>r=>e=>()=>{var n=r.id,a=r.curve,u=r.oversample;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(e.context,{curve:a,oversample:u.type})},Fe(n,r.scope,e),Oe(n,e),$e(t,n,r.parent,e)};function ob(t){return function(r){return function(){var e=t.id,n=t.cb;r.units[e].analyserOrig!==n&&(r.units[e].analyser&&r.units[e].analyser(),r.units[e].analyser=n(r.units[e].se)(),r.units[e].analyserOrig=n)}}}function ib(t){return function(r){return function(){var e=t.cb,n=t.id;if(r.units[n].recorderOrig!==e){r.units[n].recorder&&r.units[n].recorder.stop();var a=e;r.units[n].recorderOrig=e;var u=new MediaRecorder(r.units[n].se);a(u)(),u.start()}}}}function cb(t){return function(r){return function(){var e=t.id,n=t.curve;r.units[e].main.curve=n}}}function fb(t){return function(r){return function(){var e=t.id,n=t.paramName,a=t.paramValue;$R(r,r.units[e].main,n,r.units[e].controllers,a)}}}var uo=function(t,r,e){r.resume&&t.value.n!==void 0&&(r.resume[e]=t.value.n)};function lb(t){return function(r){return function(){var e=t.id,n=t.gain;ao(r,r.units[e].main,"gain",r.units[e].controllers,n),uo(n,r.units[e],"gain")}}}function _b(t){return function(r){return function(){var e=t.id,n=t.q;ao(r,r.units[e].main,"Q",r.units[e].controllers,n),uo(n,r.units[e],"Q")}}}function pb(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].resume&&(r.units[e].resume.buffer=n)}}}function sb(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].main.buffer=n}}}function mb(t){return function(r){return function(){var e=t.id,n=t.spec;r.units[e].resume&&(r.units[e].resume.spec=n)}}}function vb(t){return function(r){return function(){var e=t.id,n=t.pan;ao(r,r.units[e].main,"pan",r.units[e].controllers,n),uo(n,r.units[e],"pan")}}}function Db(t){return function(r){return function(){var e=t.id,n=t.threshold;ao(r,r.units[e].main,"threshold",r.units[e].controllers,n),uo(n,r.units[e],"threshold")}}}function db(t){return function(r){return function(){var e=t.id,n=t.loopStart;r.units[e].main.loopStart=n,r.units[e].resume.loopStart=n}}}function bb(t){return function(r){return function(){var e=t.id,n=t.loopEnd;r.units[e].main.loopEnd=n,r.units[e].resume.loopEnd=n}}}function yb(t){return function(r){return function(){var e=t.id,n=t.bufferOffset;r.units[e].resume.bufferOffset=n}}}function Ab(t){return function(r){return function(e){return function(){var n=r.id,a=r.duration;e.units[n].duration=t(void 0)(u=>u)(a)}}}}function kb(t){return function(r){return function(){var e=t.id,n=t.release;ao(r,r.units[e].main,"release",r.units[e].controllers,n),uo(n,r.units[e],"release")}}}function gb(t){return function(r){return function(){var e=t.id,n=t.offset;ao(r,r.units[e].main,"offset",r.units[e].controllers,n),uo(n,r.units[e],"offset")}}}function Cb(t){return function(r){return function(){var e=t.id,n=t.ratio;ao(r,r.units[e].main,"ratio",r.units[e].controllers,n),uo(n,r.units[e],"ratio")}}}function hb(t){return function(r){return function(){var e=t.id,n=t.attack;ao(r,r.units[e].main,"attack",r.units[e].controllers,n),uo(n,r.units[e],"attack")}}}function Eb(t){return function(r){return function(){var e=t.id,n=t.knee;ao(r,r.units[e].main,"knee",r.units[e].controllers,n),uo(n,r.units[e],"knee")}}}function Tb(t){return function(r){return function(){var e=t.id,n=t.delayTime;ao(r,r.units[e].main,"delayTime",r.units[e].controllers,n),uo(n,r.units[e],"delayTime")}}}function Sb(t){return function(r){return function(){var e=t.id,n=t.playbackRate;ao(r,r.units[e].main,"playbackRate",r.units[e].controllers,n),uo(n,r.units[e],"playbackRate")}}}function xb(t){return function(r){return function(){var e=t.id,n=t.frequency;ao(r,r.units[e].main,"frequency",r.units[e].controllers,n),uo(n,r.units[e],"frequency")}}}function Fb(t){return function(r){return function(){var e=t.id,n=t.onOff;n.x.type==="on"?MR(e)(n)(r)():n.x.type==="off"&&PR(e)(n)(r)()}}}var MR=function(t){return function(r){return function(e){return function(){if(!e.units[t].onOff){e.units[t].pendingOn=!1,e.units[t].onOff=!0,e.units[t].main=e.units[t].createClosure(e.context,e.units[t].resume);for(var n=0;n<e.units[t].audioOutgoing.length;n++){var a=e.units[t].audioOutgoing[n];e.units[t].main.connect(e.units[a].main),e.units[a].se&&e.units[t].main.connect(e.units[a].se)}e.units[t].resume&&e.units[t].resume.bufferOffset?typeof e.units[t].resume.duration=="number"?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset,e.units[t].resume.duration):e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset):e.units[t].resume&&e.units[t].resume.loopStart?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.loopStart):e.units[t].main.start(e.deprecatedWriteHead+r.o)}}}}},PR=function(t){return function(r){return function(e){return function(){if(!!e.units[t].onOff){e.units[t].onOff=!1;var n=e.units[t].main;n.addEventListener("ended",()=>{n.disconnect()}),n.stop(e.deprecatedWriteHead+r.o)}}}}};function Ob(t){for(var r=new Float32Array(t.length),e=0;e<t.length;e++)r[e]=t[e];return r}function bm(t){return function(){t.stop()}}function $b(t){return function(r){return function(e){return function(){var n=[];e.ondataavailable=function(a){n.push(a.data)},e.onstop=function(){var a=new Blob(n,{type:t});r(a)(),n=null}}}}}function wb(t){return function(r){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:r})}}}function lp(t){return function(){var r=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(r),r}}function Mb(t){return function(){var r=t.createConstantSource();return r.offset.value=0,r.connect(t.destination),r.start(),function(){r.stop(),r.disconnect(t.destination)}}}var Pb=function(t){return function(r){return function(e){return function(){for(var n=new Float32Array(r.length),a=new Float32Array(e.length),u=0;u<r.length;u++)n[u]=r[u];for(var u=0;u<e.length;u++)a[u]=e[u];return t.createPeriodicWave(n,a,{disableNormalization:!0})}}}};function kc(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},scopes:{},unsu:{},toConnect:{}}}}function Ib(t){return function(){t.close()}}function Rb(t){return function(){return fetch(t).then(function(r){return r.arrayBuffer()},function(r){return console.error("Error fetching buffer",r),Promise.reject(r)})}}function Nb(t){return function(r){return function(){return t.decodeAudioData(r)}}}function Lb(){return new(window.AudioContext||window.webkitAudioContext)}function Bb(t){return function(){return t.state}}function _p(t){return function(){return t.currentTime}}function ZE(t){return function(r){return function(e){return function(){t.then(e,r)}}}}var NR=function(t){return function(r){return vl(function(e){return df(S)(Pt(NE))(ZE(r)(function(n){return e(Kt.create(t(n)))()})(function(n){return e(Yt.create(n))()}))})}};var LR=function(t){return Va(function(r){return Yo("Promise failed, couldn't extract JS Error or String")})(rt(et))(qD(R(WD(Tv)(qu))(JD(qu)("Error")(t))(_(U_(po))(Yo)(jD(qu)(t)))))},zE=NR(LR),ym=function(t){return P(He)(Ae(Be)(t))(zE)};function Hb(t){return function(){return URL.createObjectURL(t)}}var VE=function(t){return function(r){return function(e){return Mt($b(t))(e)(function(){var n=zn(Vn)(r);return function(a){return n(Hb(a))}}())}}};var _f={ids:_(S)(jt(hp))(eo),deleteFromCache:xd,disconnectXFromY:Od,connectXToY:Fd,makeAllpass:$d(Gt),makeAnalyser:wd(Gt),makeAudioWorkletNode:Md(Gt),makeBandpass:Pd(Gt),makeConstant:Id(Gt),makeConvolver:Rd(Gt),makeDelay:Nd(Gt),makeDynamicsCompressor:Ld(Gt),makeGain:Bd(Gt),makeHighpass:Hd(Gt),makeHighshelf:Ud(Gt),makeIIRFilter:Wd(Gt),makeLoopBuf:qd(Gt),makeLowpass:Zd(Gt),makeLowshelf:zd(Gt),makeMediaElement:Vd(Gt),makeMicrophone:Gd(Gt),makeNotch:Jd(Gt),makePeaking:jd(Gt),makePeriodicOsc:Xd(Gt),makePlayBuf:Qd(Gt),makeRecorder:Kd(Gt),makeSawtoothOsc:Yd(Gt),makeSinOsc:tb(Gt),makeSpeaker:rb,makeSquareOsc:nb(Gt),makeStereoPanner:eb(Gt),makeTriangleOsc:ab(Gt),makeWaveShaper:ub(Gt),setAnalyserNodeCb:ob,setMediaRecorderCb:ib,setWaveShaperCurve:cb,setAudioWorkletParameter:fb,setBuffer:pb,setConvolverBuffer:sb,setDuration:Ab(Gt),setPeriodicOsc:mb,setOnOff:Fb,setBufferOffset:yb,setLoopStart:db,setLoopEnd:bb,setRatio:Cb,setOffset:gb,setAttack:hb,setGain:lb,setQ:_b,setPan:vb,setThreshold:Db,setRelease:kb,setKnee:Eb,setDelay:Tb,setPlaybackRate:Sb,setFrequency:xb},Tt=function(t){return function(r){return P(He)(ym(Rb(r)))(function(){var e=Nb(t);return function(n){return ym(e(n))}}())}},pp=function(t){var r=Ae(t);return function(e){return r(Bb(e))}};var ma=function(t){return Ae(t)(Lb)},oo=function(t){var r=Ae(t);return function(e){return r(Mb(e))}},Mn=function(t){return function(r){return Ae(t)(function(){var n=pp(se)(r)();return Zn($)(n!=="closed")(Ib(r))()})}},qR=mt,ZR=mt,Am=function(t){return function(r){return _(Ri)(function(e){return{microphone:function(){return t?l(Uo)(qR(e)):W.value}(),camera:function(){return r?l(Uo)(ZR(e)):W.value}()}})(ym(wb(t)(r)))}};var oi=function(){function t(){}return t.value=new t,t}(),ii=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Lu=function(){function t(){}return t.value=new t,t}(),dn=hd,Li=function(t){return Io(Un)(R(LE)(Hn(Un)(P(He)(IE(t))(Ae(Be))))(Hn(Un)(RE(Yo("We navigated away from the page"))(t))))},bl=function(t){return function(r){return function(e){return function(n){return R(t)(l(r)(Lu.value))(n)}}}},Ua=function(t){return function(r){return function(e){return function(n){return R(t)(l(r)(tt(ye)(De.value)(ne(T(n)))))(_(t.Functor0())(function(a){return tt(ye)(De.value)(ne(T(Q(ot)(a)(n))))})(_(t.Functor0())(function(a){return a.value0})(e)))}}}},km=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return _(g)(function(i){return tt(ye)(De.value)(ne(T(function(){if(i.value0 instanceof oi)return l($)(void 0);if(i.value0 instanceof ii)return Q(ot)(Q(ot)(i.value0.value0)(t(l($)(void 0))))(r(Lu.value));if(i.value0 instanceof Lu)return function(){i.value1(),r(oi.value)();var p=Ro(P(He)(ma(Be))(function(s){return P(He)(oo(Be)(s))(function(f){return P(He)(e(s))(function(m){return Ae(Be)(function(){var c=n(s)(m)(),h=Q(ot)(Q(ot)(c)(f))(Mn(se)(s));return r(new ii(h))(),h})})})}))();return t(function(){return r(Lu.value)(),ui(Li(p))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 59, column 21 - line 77, column 26): "+[i.value0.constructor.name])}())))})(Nn(Jt(V))(R(N(D))(l(C(V))(l($)(void 0)))(_(g)(function(i){return i.value0})(a)))(_(g)(nt.create)(u)))}}}}}},Wa=function(t){return function(r){return function(e){return function(){return t(e)(),r(new BE(e))()}}}},gm=function(t){return function(r){return function(e){return function(n){return function(a){return rn(function(u){return function(i){var o=bl(N(D))(C(V))(r)(i);return qf(R(N(D))(l(C(V))(tt(ms)(Xt.value)("cursor: pointer;")))(km(e)(u)(n)(a)(r)(o)))([sn(_(g)(function(p){if(p instanceof Lu)return t;if(p instanceof oi)return"\u23F3";if(p instanceof ii)return"\u{1F6D1}";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 127, column 17 - line 130, column 35): "+[p.constructor.name])})(o))])}})}}}}},wt=function(t){return function(r){return function(e){return function(n){return rn(function(a){return function(u){var i=bl(N(D))(C(V))(t)(u);return wn(km(r)(a)(e)(n)(t)(i))([sn(_(g)(function(o){if(o instanceof Lu)return"Turn on";if(o instanceof oi)return"Loading...";if(o instanceof ii)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 101, column 17 - line 104, column 42): "+[o.constructor.name])})(i))])}})}}}};var yl=function(t){return function(r){return function(){var n=kc(t)(),a=$t(of([new mc(_(g)(function(u){return Bf.create(Ek(u))})(r))])(_f))(function(u){return u(n)})();return a}}};var yt=function(t){return function(r){return function(){var n=kc(t)(),a=$t(of(r)(_f))(function(u){return u(n)})();return a}}},Cm=function(t){return function(){var e=ma(se)();return _(S)(function(n){return Q(ot)(n)(Mn(se)(e))})(yt(e)(t))()}};var zR=function(){return d.value}(),GE=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}}))(d.value)(zR)({allpass:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Ba(sr(Er)(a)(vt()))(function(u){return function(i){return Nt(lt)(.2)([u,tp(yd)(700)([tp(im(Et(ht()(J(J(Ct)(od)()()()({reflectSymbol:function(){return"q"}}))(um)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:990,q:20})([u]),tp(yd)(1110)([u,tp(im(Et(ht()(J(J(Ct)(od)()()()({reflectSymbol:function(){return"q"}}))(um)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:2010,q:30})([u])])])])}})])}}))})}}};function Bi(t){return function(e,n,a){if(n===null)return new t(e);var u=e.byteLength,i=t.BYTES_PER_ELEMENT,o=Math.min(u,n>>>0);if(a===null)return new t(e,o);var p=Math.min((u-o)/i,a);return new t(e,o,p)}}var GR=Bi(Uint8ClampedArray),JR=Bi(Uint32Array),jR=Bi(Uint16Array),JE=Bi(Uint8Array),XR=Bi(Int32Array),QR=Bi(Int16Array),KR=Bi(Int8Array),YR=Bi(Float32Array),tN=Bi(Float64Array);function jE(t){for(var r=t.length,e=new Array(r),n=0;n<r;n++)e[n]=t[n];return e}var hm={create:JE,BinaryValue0:function(){}};var Em=function(t){return function(r){return function(){return jE(r)}}};var Al=wu,kl=wu,gl=wu,Du=wu,du=wu,bu=wu,yu=wu,Au=wu;function Tm(t){return t|0}var oN=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},gc=Ht(function(t){return function(){var e=Fi(),n=ie(!0)(),a=oN("fx","FRP.Event.Animate",function(){return cr(S)(Mt(Ps)(e)(function(){var o=he(n)();return Zn($)(o)(function(){return t(void 0)(),a(19)()})()}))}),u=a(15);return u(),An(!1)(n)}});var iN="background-color: rgb(150,30,10);",cN="background-color: rgb(130,60,10);",fN="background-color: rgb(80,90,10);",lN="background-color: rgb(10,130,10);",_N="background-color: rgb(10,100,0);",pN=zs(su)(function(t){return Mr(wr(qs)()(Ka)()(B_))(iN)(Mr(wr(sa)()(Tn)()(Ka))(cN)(Mr(wr(Mu)()(Sn)()(Tn))(fN)(Mr(wr(Pu)()(xn)()(Sn))(lN)(Mr(wr(Iu)()(Ru)()(xn))(_N)(no)))))}),sN=function(t){return function(r){return function(e){return function(n){return pm(_m(Et(ht()(J(J(Ct)(lm)()()()({reflectSymbol:function(){return"fftSize"}}))(t)()()()({reflectSymbol:function(){return"cb"}})))(gt()())))({cb:n,fftSize:Ys.value})([sr(r)(e)(vt())])}}}},mN=function(){return d.value}(),Nr="background-color: rgb(255,255,255,0.0);",Hr=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return function(o){return function(p){return function(s){return function(f){return _(t)(function(m){var v=H_(r)()(H_(n)()(m)(p))(s);return v?tt(u)(Xt.value)(H_(r)()(H_(n)()(pN)(p))(s)):tt(u)(Xt.value)(Nr)})(f)}}}}}}}}}}},vN=function(){return 15/40}(),DN=function(){return 10/40}(),dN=function(){return 7/40}(),bN=function(){return 3/40}(),yN=function(){return 1/40}(),YE=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Ocarina provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Ocarina as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))(mN)({analyser:B(rn(function(n){return function(a){var u=o_(Fu(D))(rt(et))(a),i=bl(N(D))(C(V))(e)(function(p){return p.right}(u)),o=function(p){return p.left}(u);return Zr([wn(R(N(D))(l(C(V))(tt(Uf)(Xt.value)("cursor: pointer;")))(km(t)(function(p){return n(Yt.create(p))})(function(p){return Tt(p)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(p){return function(s){return function(){var m=ie(W.value)(),v=kc(p)(),c=of([sN(fm)(Er)(s)(function(ut){return function(){return An(new F(ut))(m)(),An(W.value)(m)}})])(_f),h=$t(R(N($))(_(g)(Yt.create)(c))(_(g)(Kt.create)(gc)))(function(ut){if(ut instanceof Yt)return ut.value0(v);if(ut instanceof Kt)return function(){var fr=he(m)();return Jn($)(te)(fr)(function(Zt){return function(){var re=lp(Zt)(),pt=Em(hm)(re)(),mr=ie(0)(),Fr=ie(0)(),ir=ie(0)(),Je=ie(0)(),Ho=ie(0)(),io=ie(0)(),Vi=ie(0)(),Cu=ie(0)(),bn=ie(0)(),li=ie(0)(),sf=function(Pe){if(Pe<32)return mr;if(Pe<64)return Fr;if(Pe<96)return ir;if(Pe<128)return Je;if(Pe<168)return Ho;if(Pe<160)return io;if(Pe<224)return Vi;if(oe)return Cu;throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 144, column 45 - line 152, column 63): "+[Pe.constructor.name])};ql(pt)(function(Pe){var _i=Tm(Pe);return function(){var pi=he(li)();return Nc(Br(Uu)(_i))(bn)(),Nc(Br(Uu)(_i))(sf(pi))(),Nc(Br(Uu)(1))(li)()}})();var mf=Rn(FC)($)(function(Pe){return function(){var vf=_(S)(jr)(he(Pe))(),pi=_(S)(mo(Wl)(vf))(_(S)(jr)(he(bn)))();return Mr(wr(qs)()(Ka)()(B_))(pi>vN)(Mr(wr(sa)()(Tn)()(Ka))(pi>DN)(Mr(wr(Mu)()(Sn)()(Tn))(pi>dN)(Mr(wr(Pu)()(xn)()(Sn))(pi>bN)(Mr(wr(Iu)()(Ru)()(xn))(pi>yN)(no)))))}})(Mr(wr(EC)()(RD)()(xC))(mr)(Mr(wr(TC)()(ND)()(RD))(Fr)(Mr(wr(SC)()(B_)()(ND))(ir)(Mr(wr(qs)()(Ka)()(B_))(Je)(Mr(wr(sa)()(Tn)()(Ka))(Ho)(Mr(wr(Mu)()(Sn)()(Tn))(io)(Mr(wr(Pu)()(xn)()(Sn))(Vi)(Mr(wr(Iu)()(Ru)()(xn))(Cu)(no)))))))))();return n(new Kt(mf))()}})()};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 125, column 57 - line 163, column 57): "+[ut.constructor.name])})();return function(){return h(),function(){var fr=pp(se)(p)();return Zn($)(fr!=="closed")(Mn(se)(p))()}(),n(new Kt(zs(su)(T(zs(La)(T(!1))))))()}}}})(e)(i)))([sn(_(g)(function(p){if(p instanceof Lu)return"Turn on";if(p instanceof oi)return"Loading...";if(p instanceof ii)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 176, column 31 - line 179, column 56): "+[p.constructor.name])})(i))]),hr(l(C(V))(tt(Dt)(Xt.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")))([hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Ea)(Me)(Mo)(Dt)(Ea)(Mo)(Au)(Au)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Ea)(ta)(wo)(Dt)(Ea)(wo)(yu)(Au)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Ea)(Yn)($o)(Dt)(Ea)($o)(bu)(Au)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Ea)(Kn)(Oo)(Dt)(Ea)(Oo)(du)(Au)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Ea)(Qn)(Fo)(Dt)(Ea)(Fo)(Du)(Au)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Ea)(La)(xo)(Dt)(Ea)(xo)(gl)(Au)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Ea)(ri)(So)(Dt)(Ea)(So)(kl)(Au)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Me)(Ea)(ti)(To)(Dt)(Ea)(To)(Al)(Au)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(ha)(Me)(Mo)(Dt)(ha)(Mo)(Au)(yu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(ha)(ta)(wo)(Dt)(ha)(wo)(yu)(yu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(ha)(Yn)($o)(Dt)(ha)($o)(bu)(yu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(ha)(Kn)(Oo)(Dt)(ha)(Oo)(du)(yu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(ha)(Qn)(Fo)(Dt)(ha)(Fo)(Du)(yu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(ha)(La)(xo)(Dt)(ha)(xo)(gl)(yu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(ha)(ri)(So)(Dt)(ha)(So)(kl)(yu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(ta)(ha)(ti)(To)(Dt)(ha)(To)(Al)(yu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(Ca)(Me)(Mo)(Dt)(Ca)(Mo)(Au)(bu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(Ca)(ta)(wo)(Dt)(Ca)(wo)(yu)(bu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(Ca)(Yn)($o)(Dt)(Ca)($o)(bu)(bu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(Ca)(Kn)(Oo)(Dt)(Ca)(Oo)(du)(bu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(Ca)(Qn)(Fo)(Dt)(Ca)(Fo)(Du)(bu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(Ca)(La)(xo)(Dt)(Ca)(xo)(gl)(bu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(Ca)(ri)(So)(Dt)(Ca)(So)(kl)(bu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Yn)(Ca)(ti)(To)(Dt)(Ca)(To)(Al)(bu)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(ga)(Me)(Mo)(Dt)(ga)(Mo)(Au)(du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(ga)(ta)(wo)(Dt)(ga)(wo)(yu)(du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(ga)(Yn)($o)(Dt)(ga)($o)(bu)(du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(ga)(Kn)(Oo)(Dt)(ga)(Oo)(du)(du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(ga)(Qn)(Fo)(Dt)(ga)(Fo)(Du)(du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(ga)(La)(xo)(Dt)(ga)(xo)(gl)(du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(ga)(ri)(So)(Dt)(ga)(So)(kl)(du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Kn)(ga)(ti)(To)(Dt)(ga)(To)(Al)(du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Qn)(ka)(Me)(Mo)(Dt)(ka)(Mo)(Au)(Du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Qn)(ka)(ta)(wo)(Dt)(ka)(wo)(yu)(Du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Qn)(ka)(Yn)($o)(Dt)(ka)($o)(bu)(Du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Qn)(ka)(Kn)(Oo)(Dt)(ka)(Oo)(du)(Du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Qn)(ka)(Qn)(Fo)(Dt)(ka)(Fo)(Du)(Du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Qn)(ka)(La)(xo)(Dt)(ka)(xo)(gl)(Du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Qn)(ka)(ri)(So)(Dt)(ka)(So)(kl)(Du)(o)))([]),hr(R(N(D))(l(C(V))(tt(Dt)(Xt.value)(Nr)))(Hr(g)(Qn)(ka)(ti)(To)(Dt)(ka)(To)(Al)(Du)(o)))([])])])}}))})}}};var kN=function(){return d.value}(),t0=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))(d.value)(kN)({bandpass:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Ba(sr(Er)(a)(vt()))(function(u){return function(i){return Nt(lt)(.8)([$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:400,q:1})([u]),$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:880,q:5})([u]),$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:1200,q:10})([u]),$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:2e3,q:20})([u]),$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:3e3,q:30})([u])])}})])}}))})}}};var CN=function(){return d.value}(),r0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))(CN)({compression:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([pE(Yh(Et(ht()(Ct))(gt()())))({})([sr(Er)(a)(vt())])])}}))})}}};var va=function(){return function(t){var r=fn(),e=le()({reflectSymbol:function(){return"playbackRate"}})(d.value),n=nf(t);return function(a){return r(e(n(a)))}}},pf=function(){return function(t){var r=fn(),e=le()({reflectSymbol:function(){return"onOff"}})(d.value),n=ch(t);return function(a){return r(e(n(a)))}}},e0=function(){return function(t){var r=fn(),e=le()({reflectSymbol:function(){return"offset"}})(d.value),n=nf(t);return function(a){return r(e(n(a)))}}},n0=function(){var t=fn(),r=le()({reflectSymbol:function(){return"loopStart"}})(d.value);return function(e){return t(r(e))}},a0=function(){var t=fn(),r=le()({reflectSymbol:function(){return"loopEnd"}})(d.value);return function(e){return t(r(e))}},Pn=function(){return function(t){var r=fn(),e=le()({reflectSymbol:function(){return"gain"}})(d.value),n=nf(t);return function(a){return r(e(n(a)))}}},No=function(){return function(t){var r=fn(),e=le()({reflectSymbol:function(){return"frequency"}})(d.value),n=nf(t);return function(a){return r(e(n(a)))}}};var Cl=function(){return function(t){var r=fn(),e=le()({reflectSymbol:function(){return"delayTime"}})(d.value),n=nf(t);return function(a){return r(e(n(a)))}}};var EN=function(){return d.value}(),u0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Ocarina uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>pure</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(H()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))(EN)({tf:B(pe("<|>")),txt:B(pe(`run2_
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
  ]`)),constant:B(wt(e)(t)(function(n){return l(Ta)(void 0)})(function(n){return function(a){return yt(n)([Nt(lt)(.5)([mm(am)(0)(R(N($))(vt())(l(C(G))(e0()(Bn)({d:5,o:.1,p:bo(di)(function(u){return T(function(){var i=uu(so)(u)(3)===0;return i?1:0}())})(yn(0)(1920))}))))])])}}))})}}};var SN=function(){return d.value}(),o0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))(SN)({convolution:B(wt(e)(t)(function(n){return Ut(ME)(_(Ri)(function(a){return function(u){return{loop:a,verb:u}}})(Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(Tt(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(a){return yt(n)([oE(gh)(a.verb)([sr(Er)(a.loop)(vt())])])}}))})}}};var FN=function(){return d.value}(),i0=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))(d.value)(FN)({delay:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return yt(n)([Ba(na(Ya)(a)(vt()))(function(u){return function(i){return Nt(lt)(.2)([Po(nn)(.03)([u]),Po(nn)(.1)([u]),Po(nn)(.3)([u]),Po(nn)(.7)([u])])}})])}}))})}}};var $N=function(){return d.value}(),c0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))($N)({gain:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return yt(n)([Nt(lt)(.1)([sr(Er)(a)(vt())])])}}))})}}};var MN=function(){return d.value}(),f0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))(MN)({highpass:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([_l(mu)(2e3)([sr(Er)(a)(vt())])])}}))})}}};var IN=function(){return d.value}(),l0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))(IN)({highshelf:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([sE(Kh(Et(ht()(J(J(Ct)(Fh)()()()({reflectSymbol:function(){return"gain"}}))(Oh)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:2e3,gain:.4})([sr(Er)(a)(vt())])])}}))})}}};var NN=function(){return d.value}(),_0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}}))(NN)({iirFilterEx:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([iE()()(kh(Oi)(Oi))(new nt(Vc()()(20298e-8)(Vc()()(.0004059599)(Vc()()(20298e-8)(Vv))),Vc()()(1.0126964558)(Vc()()(-1.9991880801)(Vc()()(.9873035442)(Vv)))))([sr(Er)(a)(vt())])])}}))})}}};var BN=function(){return d.value}(),p0=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))(d.value)(BN)({loopBuf:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(a){return yt(n)([sr(uf(Et(ht()(J(J(J(J(Ct)(cl)()()()({reflectSymbol:function(){return"playbackRate"}}))(j_)()()()({reflectSymbol:function(){return"loopStart"}}))(J_)()()()({reflectSymbol:function(){return"loopEnd"}}))(af)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())))({buffer:a,playbackRate:.5,loopStart:.1,loopEnd:.6})(vt()),sr(uf(Et(ht()(J(J(J(J(Ct)(cl)()()()({reflectSymbol:function(){return"playbackRate"}}))(j_)()()()({reflectSymbol:function(){return"loopStart"}}))(J_)()()()({reflectSymbol:function(){return"loopEnd"}}))(af)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())))({buffer:a,playbackRate:1,loopStart:.5,loopEnd:1.2})(vt()),sr(uf(Et(ht()(J(J(Ct)(cl)()()()({reflectSymbol:function(){return"playbackRate"}}))(af)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())))({buffer:a,playbackRate:1.7})(vt())])}}))})}}};var UN=function(){return d.value}(),s0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))(UN)({lowpass:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([pl(om)(215)([sr(Er)(a)(vt())])])}}))})}}};var qN=function(){return d.value}(),m0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))(qN)({lowshelf:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([vE(Qh(Et(ht()(J(J(Ct)(Th)()()()({reflectSymbol:function(){return"gain"}}))(Sh)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:91,gain:.4})([sr(Er)(a)(vt())])])}}))})}}};var zN=function(){return d.value}(),v0=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))(d.value)(zN)({microphone:B(wt(e)(t)(function(n){return Am(!0)(!1)})(function(n){return function(a){return yt(n)([function(){if(a.microphone instanceof F)return vu(function(u){return Nt(lt)(1)([K_(V_)(a.microphone.value0),Po(nn)(.1)([Nt(lt)(.2)([u])])])});if(a.microphone instanceof W)return Nt(lt)(.02)([lE(Ac)(440)]);throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Microphone (line 45, column 15 - line 50, column 56): "+[a.microphone.constructor.name])}()])}}))})}}};var GN=function(){return d.value}(),D0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))(GN)({notch:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([sl(ll(Et(ht()(J(J(Ct)(ol)()()()({reflectSymbol:function(){return"q"}}))(il)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:400,q:1})(l(de)(sl(ll(Et(ht()(J(J(Ct)(ol)()()()({reflectSymbol:function(){return"q"}}))(il)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:880,q:5})(l(de)(sl(ll(Et(ht()(J(J(Ct)(ol)()()()({reflectSymbol:function(){return"q"}}))(il)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:1200,q:10})(l(de)(sl(ll(Et(ht()(J(J(Ct)(ol)()()()({reflectSymbol:function(){return"q"}}))(il)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:2e3,q:20})(l(de)(sl(ll(Et(ht()(J(J(Ct)(ol)()()()({reflectSymbol:function(){return"q"}}))(il)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:3e3,q:30})(l(de)(sr(Er)(a)(vt())))))))))))])}}))})}}};var jN=function(){return d.value}(),d0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))(jN)({peaking:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([ml(fl(Et(ht()(J(J(J(Ct)(nl)()()()({reflectSymbol:function(){return"q"}}))(al)()()()({reflectSymbol:function(){return"gain"}}))(ul)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:400,q:1,gain:-20})(l(de)(ml(fl(Et(ht()(J(J(J(Ct)(nl)()()()({reflectSymbol:function(){return"q"}}))(al)()()()({reflectSymbol:function(){return"gain"}}))(ul)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:880,q:5,gain:20})(l(de)(ml(fl(Et(ht()(J(J(J(Ct)(nl)()()()({reflectSymbol:function(){return"q"}}))(al)()()()({reflectSymbol:function(){return"gain"}}))(ul)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:1200,q:10,gain:-20})(l(de)(ml(fl(Et(ht()(J(J(J(Ct)(nl)()()()({reflectSymbol:function(){return"q"}}))(al)()()()({reflectSymbol:function(){return"gain"}}))(ul)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:2e3,q:20,gain:20})(l(de)(ml(fl(Et(ht()(J(J(J(Ct)(nl)()()()({reflectSymbol:function(){return"q"}}))(al)()()()({reflectSymbol:function(){return"gain"}}))(ul)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:3e3,q:30,gain:-20})(l(de)(sr(Er)(a)(vt())))))))))))])}}))})}}};var QN=function(){return d.value}(),b0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(QN)({periodic:B(wt(e)(t)(function(n){return l(Ta)(void 0)})(function(n){return function(a){return yt(n)([Nt(lt)(.2)([Ii(Pi(Et(ht()(J(J(Ct)(Mi($i(sa)))()()()({reflectSymbol:function(){return"spec"}}))(wi)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:140,spec:new nt(Mr(wr(sa)()(Tn)()(Ka))(.1)(Mr(wr(Mu)()(Sn)()(Tn))(.2)(Mr(wr(Pu)()(xn)()(Sn))(.3)(Mr(wr(Iu)()(Ru)()(xn))(.4)(no)))),Mr(wr(sa)()(Tn)()(Ka))(.4)(Mr(wr(Mu)()(Sn)()(Tn))(.3)(Mr(wr(Pu)()(xn)()(Sn))(.2)(Mr(wr(Iu)()(Ru)()(xn))(.1)(no)))))})(vt())])])}}))})}}};var YN=function(){return d.value}(),y0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))(YN)({playBuf:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(a){return yt(n)([na(X_(Et(ht()(J(J(J(Ct)(hh)()()()({reflectSymbol:function(){return"duration"}}))(Ch)()()()({reflectSymbol:function(){return"bufferOffset"}}))(G_)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())))({buffer:a,duration:3,bufferOffset:4.2})(vt())])}}))})}}};var Ub=function(){function t(){}return t.value=new t,t}();var A0={attr:function(t){return function(r){return b({key:"controls",value:q(r)})}}};var Wb=function(){function t(){}return t.value=new t,t}();var k0={attr:function(t){return function(r){return b({key:"src",value:q(r)})}}};var qb=function(t){return function(r){return new M(X("audio")(t)(U(r)))}};var aL=function(t){return function(r){return function(e){return function(n){return kd(t)(n)(K_(r)(e))}}}},uL=function(){return d.value}(),g0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))(uL)({recorder:B(rn(function(n){return function(a){var u=o_(Fu(D))(rt(et))(a),i=o_(Fu(D))(rt(et))(function(f){return f.left}(u)),o=function(f){return f.right}(i),p=bl(N(D))(C(V))(e)(function(f){return f.right}(u)),s=function(f){return f.left}(i);return Zr([wn(R(N(D))(l(C(V))(tt(Uf)(Xt.value)("cursor: pointer;")))(_(g)(function(f){return tt(ye)(De.value)(ne(T(function(){if(f.e instanceof oi)return l($)(void 0);if(f.e instanceof ii)return Q(ot)(Q(ot)(Q(ot)(f.e.value0)(t(l($)(void 0))))(Jn($)(te)(f.rec)(function(){var m=O_(yD);return function(v){return m(bm(v))}}())))(n(Yt.create(Lu.value)));if(f.e instanceof Lu)return function(){f.cncl();var v=Q_();n(new Yt(oi.value))();var c=Ro(P(He)(_(Ri)(function(h){return h.microphone})(Am(!0)(!1)))(function(h){return Ae(Be)(function(){var dt=Gt(l($)(l($)(void 0)))(function(fr){return function(){var Kr=ma(se)(),re=kc(Kr)(),pt=of([aL(ed)(V_)(fr)(function(Fr){return function(){return n(new Kt(new Yt(Fr)))(),cr(S)(cm(Fr)(v))(),VE("audio/ogg; codecs=opus")(function(Je){return n(Kt.create(Kt.create(Je)))})(Fr)()}})])(_f),mr=$t(pt)(function(Fr){return Fr(re)})();return function(){mr(),P(Vn)(aE(v))(ce($)(te)(function(){var Je=O_(yD);return function(Ho){return Je(bm(Ho))}}()))();var ir=pp(se)(Kr)();return Zn($)(ir!=="closed")(Mn(se)(Kr))()}}})(h)();return n(new Yt(new ii(dt)))(),dt})}))();return t(function(){return n(Yt.create(Lu.value))(),ui(Li(c))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 65, column 47 - line 108, column 52): "+[f.e.constructor.name])}())))})(Nn(Jt(V))(R(N(D))(l(C(V))(W.value))(_(g)(F.create)(o)))(_(g)(Df)(Nn(Jt(V))(R(N(D))(l(C(V))(l($)(void 0)))(_(g)(function(f){return f.value0})(e)))(_(g)(function(f){return function(m){return function(v){return{e:f,cncl:m,rec:v}}}})(p)))))))([sn(_(g)(function(f){if(f instanceof Lu)return"Turn on";if(f instanceof oi)return"Loading...";if(f instanceof ii)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 119, column 31 - line 122, column 56): "+[f.constructor.name])})(p))]),Zr([qb(R(N(D))(l(C(V))(tt(A0)(Ub.value)("true")))(R(N(D))(l(C(V))(tt(uD)(Xt.value)("display:none;")))(R(N(D))(_(g)(function(f){return tt(k0)(Wb.value)(f)})(s))(_(g)(T(tt(uD)(Xt.value)("display:block;")))(s)))))([])])])}}))})}}};var iL=function(){return d.value}(),C0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(iL)({periodic:B(wt(e)(t)(function(n){return l(Ta)(void 0)})(function(n){return function(a){return yt(n)([Nt(lt)(.2)([fE(Ah)(448)(vt())])])}}))})}}};var fL=function(){return d.value}(),h0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(fL)({periodic:B(wt(e)(t)(function(n){return l(Ta)(void 0)})(function(n){return function(a){return yt(n)([Nt(lt)(.2)([cf(Ac)(448)(vt())])])}}))})}}};var _L=function(){return d.value}(),E0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(_L)({periodic:B(wt(e)(t)(function(n){return l(Ta)(void 0)})(function(n){return function(a){return yt(n)([Nt(lt)(.2)([Y_(el)(448)(vt())])])}}))})}}};var sL=function(){return d.value}(),T0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))(sL)({pan:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return yt(n)([DE(yh)(1)([sr(Er)(a)(vt())])])}}))})}}};var vL=function(){return d.value}(),S0=It({reflectType:function(){return`<ul>
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
`}})()()(Y)(vL)({});var dL=function(){return d.value}(),x0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(H()(Y)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(dL)({periodic:B(wt(e)(t)(function(n){return l(Ta)(void 0)})(function(n){return function(a){return yt(n)([Nt(lt)(.2)([sm(nm)(448)(vt())])])}}))})}}};var yL=function(){return d.value}(),F0=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(H()(H()(Y)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}}))(yL)({code:B(pe(`do
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
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]`)),waveShaper:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){var u=function(i){var o=Dc/180;return _($r)(function(p){var s=jr(p)*2/jr(44100)-1;return(3+i)*s*20*o/(Dc+i*Xm(za)(wc)(s))})(yn(0)(44099))};return yt(n)([dE(Xh)(Ob(u(400)))([sr(Er)(a)(vt())])])}}))})}}};var kL=function(){return d.value}(),O0=function(t){return function(r){return function(e){return function(n){var a=Q(ot)(r(lf.value))(dn),u=Wa(t)(e);return It({reflectType:function(){return`<div>
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
</div>`}})()()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(mn()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(H()(Y)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}}))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}}))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}}))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}}))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}}))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}}))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}}))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}}))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}}))(kL)({drumroll:B(gm("\u{1F941}")(n)(u)(function(i){return Tt(i)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(i){return function(o){return yt(i)([Nt(lt)(1)([sr(Er)(o)(vt())])])}})),toc:B(S0),allpass:B(GE(u)(r)(n)),analyser:B(YE(u)(r)(n)),bandpass:B(t0(u)(r)(n)),constant:B(u0(u)(r)(n)),compression:B(r0(u)(r)(n)),convolution:B(o0(u)(r)(n)),delay:B(i0(u)(r)(n)),gain:B(c0(u)(r)(n)),highpass:B(f0(u)(r)(n)),highshelf:B(l0(u)(r)(n)),iirFilter:B(_0(u)(r)(n)),loopBuf:B(p0(u)(r)(n)),lowshelf:B(m0(u)(r)(n)),lowpass:B(s0(u)(r)(n)),notch:B(D0(u)(r)(n)),playBuf:B(y0(u)(r)(n)),peaking:B(d0(u)(r)(n)),microphone:B(v0(u)(r)(n)),pan:B(T0(u)(r)(n)),periodicOsc:B(b0(u)(r)(n)),recorder:B(g0(u)(r)(n)),sawtoothOsc:B(C0(u)(r)(n)),sinOsc:B(h0(u)(r)(n)),squareOsc:B(E0(u)(r)(n)),triangleOsc:B(x0(u)(r)(n)),waveShaper:B(F0(u)(r)(n)),next:Ua(N(D))(C(V))(n)(a)})}}}};var Zb=function(){function t(){}return t.value=new t,t}(),$0={attr:function(t){return function(r){return b({key:"checked",value:q(r)})}}};var Lo=function(){function t(){}return t.value=new t,t}();var ci={attr:function(t){return function(r){return b({key:"type",value:q(r)})}}};var Bo=function(t){return function(r){return new M(X("input")(t)(U(r)))}};var EL=function(t){return t},xm=function(t){return function(r){return function(e){return Ci(t)(R(t.Alternative0().Plus1().Alt0())(l(t.Alternative0().Applicative0())(r))(e))}}};var vp=function(t){return function(r){return t(r)}},Cc=function(t){return{map:function(r){return function(e){return function(n){return e(_(t)(function(a){return function(u){return a(r(u))}})(n))}}}}},Hi=function(t){return function(r){return function(e){return function(n){return vp(_(Cc(t))(r)(e))(_(t)(ji)(n))}}}};var hl=function(t){return Hi(t)(T)};var ku=EL;var w0=function(t){return function(r){return function(e){return ku(function(n){return xe(t)(R(t.Alternative0().Plus1().Alt0())(l(t.Alternative0().Applicative0())(vp(r)(n)))(_(t.Filterable1().Functor1())(function(a){return vp(a)(n)})(e)))})}}},zb=function(t){return{apply:function(r){return function(e){return function(n){return e(r(_(t)(Hu(si))(n)))}}},Functor0:function(){return Cc(t)}}};var El=function(t){return function(r){return Ht(function(e){return $t(r)(function(n){return function(){var u=_p(t)();return e({acTime:u,value:n})()}})})}};var M0=function(t){return function(r){return function(e){var n=function(a){return function(u){return function(i){return function(o){return function(p){return function(s){return function(){var m=he(i)();return Zn($)(m)(function(){var c=_p(t)(),h=Yp(Sg(Wu(za)(u-c-.04)(.01)*1e3))(function(){var dt=he(i)();return Zn($)(dt)(function(){return An(u)(p)(),a(u)(),n(a)(u+s)(i)(o)(p)(s)()})()})();return An(new F(h))(o)()})()}}}}}}};return Ht(function(a){return function(){var i=ie(!0)(),o=ie(W.value)(),p=_p(t)(),s=ie(p+r)();n(a)(r)(i)(o)(s)(r)();var f=$t(e)(function(m){return function(){P(Vn)(he(o))(ce($)(te)(__))();var c=he(s)();return n(a)(c+m)(i)(o)(s)(m)()}})();return Q(ot)(Q(ot)(f)(An(!1)(i)))(P(Vn)(he(o))(ce($)(te)(__)))}})}}};var qa=function(t){return function(r){return function(e){return function(n){return function(a){var u=e===t||n===r;if(u)return r;var i=(n-r)/(e-t),o=r-i*t;return i*a+o}}}}};var TL=function(){return d.value}(),P0=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<section>
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

</section>`}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}}))(TL)({txt:B(pe(`module Main where

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
import FRP.Event.Class (fold, mapAccum, sampleOn)
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
  )`)),empl:B(to()()(Wr({reflectSymbol:function(){return"cbx"}})(qr(Yu(pu()(Wr({reflectSymbol:function(){return"cbx0"}})(qr(_e))(Wr({reflectSymbol:function(){return"cbx1"}})(qr(_e))(Wr({reflectSymbol:function(){return"cbx2"}})(qr(_e))(Wr({reflectSymbol:function(){return"cbx3"}})(qr(_e))(Xn)()())()())()())()()))))(Wr({reflectSymbol:function(){return"startStop"}})(qr(Yu(pu()(Wr({reflectSymbol:function(){return"start"}})(qr(_e))(Wr({reflectSymbol:function(){return"stop"}})(qr(_e))(Xn)()())()()))))(Xn)()())()())(ju({reflectSymbol:function(){return"cbx"}})()()()(fe({reflectSymbol:function(){return"cbx0"}})()()(fe({reflectSymbol:function(){return"cbx1"}})()()(fe({reflectSymbol:function(){return"cbx2"}})()()(fe({reflectSymbol:function(){return"cbx3"}})()()(jn)()())()())()())()())(ju({reflectSymbol:function(){return"startStop"}})()()()(fe({reflectSymbol:function(){return"start"}})()()(fe({reflectSymbol:function(){return"stop"}})()()(jn)()())()())(jn)()())()())(d.value)(function(a){return function(u){var i=R(N(D))(l(C(V))(void 0))(u.startStop.start),o=function(v){return xm(Jt(G))(!1)(Gu(Jt(G))(T(fu(Ga)))(_u(v))(!1))},p=o(u.cbx.cbx3),s=o(u.cbx.cbx2),f=o(u.cbx.cbx1),m=o(u.cbx.cbx0);return Zr([wn(gn(Bt)(E(D))(_(g)(function(){var v=tt(ye)(De.value);return function(c){return v(ne(T(c)))}}()))([Vr(g)(Nn(Jt(V))(R(N(D))(l(C(V))(l($)(void 0)))(_(g)(function(v){return v.value0})(n)))(K(g)(i)(rt(et))))(function(v){return function(){v();var h=ma(se)(),ut=oo(se)(h)(),dt=function(Kr){return function(re){return function(pt){return p_(Jt(G))(function(mr){return function(Fr){var ir=Fr.value1+(mr.value1-Fr.value0)*function(){return mr.value0?Kr:1}();return new nt(new nt(mr.value1,ir),ir)}})(Hi(g)(nt.create)(re)(pt))(new nt(0,0))}}},fr=yl(h)(_c(G)(_(g)(function(){var Kr=Br(Fa)(.04);return function(re){return Kr(function(pt){return pt.acTime}(re))}}())(El(h)(gc)))(function(Kr){var re=function(Je){return function(Ho){return Ci(Jt(G))(Kr)(_(g)(Df)(Ci(Jt(G))(Ho)(_(g)(function(io){return function(Vi){return function(Cu){return{f:io,a:Vi,t:Cu}}}})(Je))))}},pt=_(g)(function(Je){return Je?4:1})(hl(g)(p)(Kr)),mr=dt(4)(s)(Kr),Fr=_(g)(function(Je){return Je?4:1})(hl(g)(f)(Kr)),ir=dt(8)(m)(Kr);return[Ge(lt)(0)(Vr(g)(re(ir)(Fr))(function(Je){return Pn()(We)({n:qa(1)(.01)(4)(.15)(Je.a)*ds(Dc*Je.f)+.15,o:Je.t,t:ei})}))([Ii(Pi(Et(ht()(J(J(Ct)(Mi($i(sa)))()()()({reflectSymbol:function(){return"spec"}}))(wi)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:325.6,spec:new nt(Mr(wr(sa)()(Tn)()(Ka))(.3)(Mr(wr(Mu)()(Sn)()(Tn))(-.1)(Mr(wr(Pu)()(xn)()(Sn))(.7)(Mr(wr(Iu)()(Ru)()(xn))(-.4)(no)))),Mr(wr(sa)()(Tn)()(Ka))(.6)(Mr(wr(Mu)()(Sn)()(Tn))(.3)(Mr(wr(Pu)()(xn)()(Sn))(.2)(Mr(wr(Iu)()(Ru)()(xn))(0)(no)))))})(Se(Bt)(E($))([vt(),Vr(g)(re(mr)(pt))(function(Je){return No()(We)({n:325.6+qa(1)(3)(4)(15.5)(Je.a)*ds(Dc*Je.f),o:Je.t,t:ei})})]))])]}))(),Zt=Q(ot)(Q(ot)(fr)(ut))(Mn(se)(h));return t(Q(ot)(Zt)(a.startStop.start(void 0)))(),a.startStop.stop(Zt)()}}),Vr(g)(u.startStop.stop)(function(v){return Q(ot)(v)(Q(ot)(t(l($)(void 0)))(a.startStop.start(void 0)))})]))([sn(Se(Bt)(E(D))([K(g)(i)("Turn on"),K(g)(u.startStop.stop)("Turn off")]))]),hr(gn(Bt)(E(D))(_(g)(tt(Dt)(Xt.value)))([K(g)(u.startStop.stop)("display:block;"),K(g)(i)("display:none;")]))(_($r)(function(v){return Bo(Se(Bt)(E(D))([l(C(V))(tt(ci)(Lo.value)("checkbox")),l(C(V))(tt(ye)(De.value)(ne(T(v(void 0))))),K(g)(i)(tt($0)(Zb.value)("false"))]))([])})(Wm($r)([function(v){return v.cbx0},function(v){return v.cbx1},function(v){return v.cbx2},function(v){return v.cbx3}])(a.cbx)))])}}))})}}}};var Vb={recip:function(t){return 1/t},Ring0:function(){return wc}};var Gb=function(t){return function(r){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return r}}}};function Tl(t){return function(){return function(r){return t(r)()}}}function Sl(t){return function(r){return function(e){return function(n){return function(){return n.addEventListener(t,r,e)}}}}}function xl(t){return function(r){return function(e){return function(n){return function(){return n.removeEventListener(t,r,e)}}}}}function Jb(t){return t.clientX}function jb(t){return t.clientY}function Dp(t){return t.button}var dp=Rt("MouseEvent");var I0=function(t){return function(r){return Ht(function(e){return $t(r)(function(n){return function(){var u=he(t.buttons)();return e({value:n,buttons:u})()}})})}};var R0=function(){var r=ie(W.value)(),e=ie(Iv)(),n=_(S)($D)(Fi)(),a=Tl(function(p){return ce($)(te)(function(s){return An(new F({x:Jb(s),y:jb(s)}))(r)})(dp(p))})(),u=Tl(function(p){return ce($)(te)(function(s){return Ef(_k(Ye)(Dp(s)))(e)})(dp(p))})(),i=Tl(function(p){return ce($)(te)(function(s){return Ef(Kp(Ye)(Dp(s)))(e)})(dp(p))})();Sl(fn()("mousemove"))(a)(!1)(n)(),Sl(fn()("mousedown"))(u)(!1)(n)(),Sl(fn()("mouseup"))(i)(!1)(n)();var o=function(){return xl(fn()("mousemove"))(a)(!1)(n)(),xl(fn()("mousedown"))(u)(!1)(n)(),xl(fn()("mouseup"))(i)(!1)(n)()};return{position:r,buttons:e,dispose:o}},N0=Ht(function(t){return function(){var e=_(S)($D)(Fi)(),n=Tl(function(a){return ce($)(te)(function(u){return t(Dp(u))})(dp(a))})();return Sl(fn()("mousedown"))(n)(!1)(e)(),xl(fn()("mousedown"))(n)(!1)(e)}});var B0=function(t){return ku(function(r){return _(g)(function(e){return e.value(e.buttons)})(I0(t)(r))})};var Kb=function(t){return t};function $m(){return Date.now()}var lT=function(t){return Ht(function(r){return $t(t)(function(e){return function(){var a=$m();return r({time:a,value:e})()}})})};var c1=ku(function(t){return _(g)(function(r){return r.value(r.time)})(lT(t))}),ty=_(Cc(g))(function(){var t=np($E);return function(r){return t(Kb(r))}}())(c1);var l1=function(t){var r=function(u){return function(i){return function(o){return function(p){return function(s){return function(f){return function(m){var v=Br(i.DivisionRing1().Ring0().Semiring0())(ba(i.DivisionRing1().Ring0().Semiring0()))(ba(i.DivisionRing1().Ring0().Semiring0())),c=function(h){return function(ut){if(h.last instanceof W)return ut;if(h.last instanceof F)return Br(o)(ut)(p(function(dt){return mo(i.EuclideanRing0())(In(i.DivisionRing1().Ring0().Semiring0())(dt(Br(o)(h.last.value0.value1)(h.now.value1)))(Eu(i.DivisionRing1().Ring0())(h.now.value0)(h.last.value0.value0)))(v)}));throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 103, column 5 - line 103, column 35): "+[h.constructor.name,ut.constructor.name])}};return ku(function(h){var ut=vp(m)(K(u.Filterable1().Functor1())(h)(rt(et))),dt=rs(u)(Hi(u.Filterable1().Functor1())(nt.create)(f)(ut)),fr=Gu(u)(c)(dt)(s);return Ci(u)(fr)(h)})}}}}}}},e=function(u){return function(i){return r(u)(i)(i.DivisionRing1().Ring0().Semiring0())(function(o){return o(rt(et))})}},n=function(u){return function(i){return ku(function(o){return s_(Jt(G))(function(p){var s=i(xm(Jt(G))(u)(p));return{input:hl(g)(s)(o),output:Ci(Jt(G))(p)(o)}})})}},a=function(u){return function(i){return function(o){if(lk(u))return-8*(i-1)-o*2;if(oe)return 2*(4-i);throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 63, column 3 - line 65, column 34): "+[u.constructor.name,i.constructor.name,o.constructor.name])}}};return n(2)(function(u){return e(Jt(G))(Gb(Wl)(Vb))(2)(_(Cc(g))(Ee())(ty))(function(){var i=n(10)(function(o){return e(Jt(G))(Gb(Wl)(Vb))(10)(_(Cc(g))(Ee())(ty))(Ut(zb(g))(Ut(zb(g))(_(Cc(g))(a)(B0(t)))(u))(o))});return w0(Jt(G))(i)(K(g)(N0)(i))}())})},_1=function(){return d.value}(),_T=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in ocarina that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}}))(_1)({txt:B(pe(`module Main

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
import FRP.Event.Class (class IsEvent, fix, fold, sampleOn, withLast)
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
  )`)),empl:B(to()()(Wr({reflectSymbol:function(){return"start"}})(qr(_e))(Wr({reflectSymbol:function(){return"stop"}})(qr(_e))(Xn)()())()())(fe({reflectSymbol:function(){return"start"}})()()(fe({reflectSymbol:function(){return"stop"}})()()(jn)()())()())(d.value)(function(a){return function(u){var i=R(N(D))(l(C(V))(void 0))(u.start);return Zr([wn(gn(Bt)(E(D))(_(g)(function(){var o=tt(ye)(De.value);return function(p){return o(ne(T(p)))}}()))([Vr(g)(Nn(Jt(V))(R(N(D))(l(C(V))(l($)(void 0)))(_(g)(function(o){return o.value0})(n)))(K(g)(i)(rt(et))))(function(o){return function(){o();var s=ma(se)(),f=oo(se)(s)(),m=R0(),v=E_(0)(1e4)(),c=function(pt){return{o:pt.value0+.04,n:pt.value1,t:ei}},h=_(Eo)(function(pt){return pt-.5})(N_(eC)),ut=P(Qc)(h)(function(pt){return P(Qc)(h)(function(mr){return P(Qc)(h)(function(Fr){return P(Qc)(h)(function(ir){return l(I_)(Mr(wr(sa)()(Tn)()(Ka))(pt)(Mr(wr(Mu)()(Sn)()(Tn))(mr)(Mr(wr(Pu)()(xn)()(Sn))(Fr)(Mr(wr(Iu)()(Ru)()(xn))(ir)(no)))))})})})}),dt=Ut(Kc)(_(Eo)(nt.create)(ut))(ut),fr=Ut(Kc)(Ut(Kc)(Ut(Kc)(_(Eo)(function(pt){return function(mr){return function(Fr){return function(ir){return{s0:pt,s1:mr,s2:Fr,s3:ir}}}}})(dt))(dt))(dt))(dt),Zt=Xf(fr)({newSeed:Vf(v),size:5}),Kr=yl(s)(_c(G)(_(g)(function(pt){return new nt(pt.acTime,pt.value)})(El(s)(hl(g)(l1(m))(gc))))(function(pt){return[Ge(lt)(0)(_(g)(function(){var mr=Pn()(We),Fr=ra(Fn)(function(ir){return Wu(za)(-.4)(.5*(ir-1))});return function(ir){return mr(c(Fr(ir)))}}())(pt))([pl(vd(Et(ht()(J(J(Ct)(xh)()()()({reflectSymbol:function(){return"q"}}))(nd)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4,q:20})([_E(el)(90.4)])]),Ge(lt)(0)(_(g)(function(){var mr=Pn()(We),Fr=ra(Fn)(function(ir){return Wu(za)(-.2)(.4*(ir-3))});return function(ir){return mr(c(Fr(ir)))}}())(pt))([$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4*4,q:20})([Ii(Pi(Et(ht()(J(J(Ct)(Mi($i(sa)))()()()({reflectSymbol:function(){return"spec"}}))(wi)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4*3.02,spec:Zt.s0})(R(N($))(vt())(_(g)(function(){var mr=No()(We),Fr=ra(Fn)(function(ir){return 90.4*3.02+14*(ir-1)});return function(ir){return mr(c(Fr(ir)))}}())(pt)))])]),Ge(lt)(0)(_(g)(function(){var mr=Pn()(We),Fr=ra(Fn)(function(ir){return Wu(za)(-.1)(.2*(ir-6))});return function(ir){return mr(c(Fr(ir)))}}())(pt))([$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4*6,q:20})([Ii(Pi(Et(ht()(J(J(Ct)(Mi($i(sa)))()()()({reflectSymbol:function(){return"spec"}}))(wi)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4*5.07,spec:Zt.s1})(R(N($))(vt())(_(g)(function(){var mr=No()(We),Fr=ra(Fn)(function(ir){return 90.4*5.07+18*(ir-1)});return function(ir){return mr(c(Fr(ir)))}}())(pt)))])]),Ge(lt)(0)(_(g)(function(){var mr=Pn()(We),Fr=ra(Fn)(function(ir){return Wu(za)(0)(.2*(ir-3))});return function(ir){return mr(c(Fr(ir)))}}())(pt))([$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4*8,q:20})([Ii(Pi(Et(ht()(J(J(Ct)(Mi($i(sa)))()()()({reflectSymbol:function(){return"spec"}}))(wi)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4*7.13,spec:Zt.s2})(R(N($))(vt())(_(g)(function(){var mr=No()(We),Fr=ra(Fn)(function(ir){return 90.4*7.13+32*(ir-1)});return function(ir){return mr(c(Fr(ir)))}}())(pt)))])]),Ge(lt)(0)(_(g)(function(){var mr=Pn()(We),Fr=ra(Fn)(function(ir){return Wu(za)(0)(.1*(ir-7))});return function(ir){return mr(c(Fr(ir)))}}())(pt))([Ii(Pi(Et(ht()(J(J(Ct)(Mi($i(sa)))()()()({reflectSymbol:function(){return"spec"}}))(wi)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:90.4*9.14,spec:Zt.s3})(R(N($))(vt())(_(g)(function(){var mr=No()(We),Fr=ra(Fn)(function(ir){return 90.4*9.14+31*(ir-1)});return function(ir){return mr(c(Fr(ir)))}}())(pt)))])]}))(),re=Q(ot)(Q(ot)(Kr)(f))(Mn(se)(s));return t(Q(ot)(re)(a.start(void 0)))(),a.stop(re)()}}),Vr(g)(u.stop)(function(o){return Q(ot)(o)(Q(ot)(t(l($)(void 0)))(a.start(void 0)))})]))([sn(Se(Bt)(E(D))([K(g)(i)("Turn on"),K(g)(u.stop)("Turn off")]))])])}}))})}}}};var s1=function(){return d.value}(),pT=function(t){return function(r){return function(e){return function(n){var a=Wa(t)(e);return It({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(H()(H()(mn()(Y)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}}))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}}))(s1)({next:Ua(N(D))(C(V))(n)(Q(ot)(r(cp.value))(dn)),fold:B(P0(a)(r)(e)(n)),fix:B(_T(a)(r)(e)(n))})}}}};var v1=function(){function t(){}return t.value=new t,t}(),sT=function(){function t(){}return t.value=new t,t}(),ry=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),D1=`module Main where

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
`;var d1=function(){return d.value}(),b1=function(t){return function(r){return function(e){return l(t)(pf(r)(ef)({x:rd,o:e}))}}},y1=function(t){return function(r){return function(e){return l(t)(pf(r)(ef)({x:Dh,o:e}))}}},A1=Ra(pn)(jr)(function(t){var r=function(a){return R(N($))(b1(C(G))()(a+.27*(t*vc(1.005)(t))))(y1(C(G))()(a+3+.3*(t*vc(1.005)(t))))},e=function(a){return l(C(G))(Pn()(Bn)({p:[0,.4,.1,.05,.01,0],o:a+.3*(t*vc(1.005)(t)),d:.8}))},n=function(a){return function(u){return Ge(lt)(0)(e(a))([cf(Ac)(200+t*u)(r(a))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),mT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}))(d.value)(d1)({txt:B(pe(D1)),ex0:B(rn(function(n){return Ra(pn)(function(a){return R(N(D))(l(C(V))(v1.value))(a)})(function(a){return Zr([wn(Vr(g)(Nn(Jt(V))(R(N(D))(l(C(V))(l($)(void 0)))(_(g)(function(u){return u.value0})(e)))(_(g)(nt.create)(a)))(function(u){return tt(ye)(De.value)(ne(T(function(){return u.value0 instanceof ry?Q(ot)(Q(ot)(u.value0.value0)(n(sT.value)))(t(l($)(void 0))):function(){u.value1();var o=Cm([Nt(lt)(1)(eu(mi)(_($r)(A1)(yn(0)(100))))])();return t(Q(ot)(o)(n(sT.value)))(),n(new ry(o))()}}())))}))([sn(Vr(g)(a)(function(u){return u instanceof ry?"Turn off":"Turn on"}))])])})}))})}}};var Ui=function(){function t(){}return t.value=new t,t}();var Ec={attr:function(t){return function(r){return b({key:"max",value:q(r)})}}};var Wi=function(){function t(){}return t.value=new t,t}();var Tc={attr:function(t){return function(r){return b({key:"min",value:q(r)})}}};var qi=function(){function t(){}return t.value=new t,t}();var Sc={attr:function(t){return function(r){return b({key:"input",value:st(r)})}}};var Zi=function(){function t(){}return t.value=new t,t}(),xc={attr:function(t){return function(r){return b({key:"step",value:q(r)})}}};var zi=function(){function t(){}return t.value=new t,t}();var Fc={attr:function(t){return function(r){return b({key:"value",value:q(r)})}}};var fi=function(t){return function(r){return function(e){return R(t)(r)(e(void 0))}}};var g1=Rg,gu={convert:function(t){return t}},bp={convert:function(t){return x_(t)}},DT=function(t){return t},ey=function(t){return t.convert},ru=function(t){return function(r){return function(e){return bt(g1)(x_(r))(ey(t)(e(void 0)))}}};var yp=function(t){return function(r){return function(e){return function(n){return gn(Ng)(r)(e)(DT(ey(t)(n)))}}}};function bT(t){return t.target}var Fl=function(t){return en(bT(t))};var E1=`module Main where

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
`,T1=function(){return d.value}(),S1="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",yT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(H()(H()(Y)({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}}))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(d.value)(T1)({wagtxt:B(pe(`run2_
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
          (add <$> (pure 0.0 <|> sl1))`)),txt:B(pe(E1)),ex1:B(to()()(Wr({reflectSymbol:function(){return"slider"}})(qr(Yu(pu()(Wr({reflectSymbol:function(){return"s0"}})(qr(_e))(Wr({reflectSymbol:function(){return"s1"}})(qr(_e))(Wr({reflectSymbol:function(){return"s2"}})(qr(_e))(Xn)()())()())()()))))(Wr({reflectSymbol:function(){return"startStop"}})(qr(Yu(pu()(Wr({reflectSymbol:function(){return"loading"}})(qr(_e))(Wr({reflectSymbol:function(){return"start"}})(qr(_e))(Wr({reflectSymbol:function(){return"stop"}})(qr(_e))(Xn)()())()())()()))))(Xn)()())()())(ju({reflectSymbol:function(){return"slider"}})()()()(fe({reflectSymbol:function(){return"s0"}})()()(fe({reflectSymbol:function(){return"s1"}})()()(fe({reflectSymbol:function(){return"s2"}})()()(jn)()())()())()())(ju({reflectSymbol:function(){return"startStop"}})()()()(fe({reflectSymbol:function(){return"loading"}})()()(fe({reflectSymbol:function(){return"start"}})()()(fe({reflectSymbol:function(){return"stop"}})()()(jn)()())()())()())(jn)()())()())(d.value)(function(n){return function(a){var u=R(N(D))(a.startStop.start)(l(C(V))(void 0)),i=_u(a.slider.s2),o=_u(a.slider.s1),p=_u(a.slider.s0),s=function(f){return sr(uf(Et(ht()(J(J(J(J(Ct)(cl)()()()({reflectSymbol:function(){return"playbackRate"}}))(j_)()()()({reflectSymbol:function(){return"loopStart"}}))(J_)()()()({reflectSymbol:function(){return"loopEnd"}}))(af)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())))({buffer:f,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(fi(N($))(vt())(function(){return fi(N($))(_(g)(function(){var m=va()(em),v=qa(0)(.2)(100)(5);return function(c){return m(v(c))}}())(p))(function(){return fi(N($))(_(g)(function(){var m=n0(),v=qa(0)(0)(100)(1.2);return function(c){return m(v(c))}}())(o))(function(){return _(g)(function(){var m=a0(),v=qa(0)(.05)(100)(1);return function(c){return m(v(c))}}())(Nn(Jt(G))(i)(_(g)(Br(Fa))(R(N($))(l(C(G))(0))(o))))})})}))};return Zr(bt(un)(_($r)(function(f){return Zr([pe(f.l),Bo(yp(gu)(E(D))(l(C(V)))(ru(gu)(tt(ci)(Lo.value)("range"))(function(){return ru(gu)(tt(Tc)(Wi.value)("0"))(function(){return ru(gu)(tt(Ec)(Ui.value)("100"))(function(){return ru(gu)(tt(xc)(Zi.value)("1"))(function(){return ru(bp)(tt(Fc)(zi.value)("50"))(function(){return tt(Sc)(qi.value)(ne(function(){var m=ce($)(te)(bf(Vn)(Yc)(f.f)),v=zn(Oa)(Qf);return function(c){return m(v(Fl(c)))}}()))})})})})})))([])])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([wn(yp(gu)(E(D))(_(g)(function(){var f=tt(ye)(De.value);return function(m){return f(ne(T(m)))}}()))(ru(gu)(K(g)(a.startStop.loading)(l($)(void 0)))(function(){return ru(bp)(Vr(g)(a.startStop.stop)(function(f){return Q(ot)(f)(Q(ot)(t(l($)(void 0)))(n.startStop.start(void 0)))}))(function(){return Vr(g)(Nn(Jt(V))(R(N(D))(l(C(V))(l($)(void 0)))(_(g)(function(f){return f.value0})(e)))(K(g)(u)(rt(et))))(function(f){return function(){f(),n.startStop.loading(void 0)();var v=Ro(P(He)(ma(Be))(function(c){return P(He)(oo(Be)(c))(function(h){return P(He)(Tt(c)(S1))(function(ut){return Ae(Be)(function(){var fr=yt(c)([s(ut)])(),Zt=Q(ot)(Q(ot)(fr)(h))(Mn(se)(c));return n.startStop.stop(Zt)(),Zt})})})}))();return t(function(){return n.startStop.start(void 0)(),ui(Li(v))()})(),void 0}})})})))([sn(fi(N(D))(_(g)(T("Turn off"))(a.startStop.stop))(function(){return _(g)(T("Turn on"))(u)}))])]))}}))})}}};var F1=`module Main where

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
                        $ toEvent event.slider
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
  )`,O1=ku(function(t){return Ht(function(r){return $t(t)(function(e){return function(){var a=eo();return r(e(a))()}})})}),$1=function(){return d.value}(),w1=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(oe)return 587.329536;throw new Error("Failed pattern match at Ocarina.Example.Docs.Events.Ex2 (line 227, column 1 - line 227, column 23): "+[t.constructor.name])},AT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))(d.value)($1)({txt:B(pe(F1)),ex2:B(to()()(Wr({reflectSymbol:function(){return"slider"}})(qr(_e))(Wr({reflectSymbol:function(){return"startStop"}})(qr(Yu(pu()(Wr({reflectSymbol:function(){return"start"}})(qr(_e))(Wr({reflectSymbol:function(){return"stop"}})(qr(_e))(Xn)()())()()))))(Xn)()())()())(fe({reflectSymbol:function(){return"slider"}})()()(ju({reflectSymbol:function(){return"startStop"}})()()()(fe({reflectSymbol:function(){return"start"}})()()(fe({reflectSymbol:function(){return"stop"}})()()(jn)()())()())(jn)()())()())(d.value)(function(n){return function(a){var u=R(N(D))(a.startStop.start)(l(C(V))(void 0)),i=function(o){return _c(G)(o)(function(p){var s=_(g)(function(){var ut=Br(Fa)(.01);return function(dt){return ut(cn(dt))}}())(p),f=_(g)(Ja)(p),m=R(N($))(vt())(_(g)(function(){var ut=No()(em);return function(dt){return ut(w1(dt))}}())(f)),v=_(g)(function(ut){return tm(function(dt){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:dt}}(ut))})(s),c=_(g)(function(ut){return tm(function(dt){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:dt}}(ut))})(s),h=_(g)(function(ut){return tm(function(dt){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:dt}}(ut))})(s);return[Ba(sm(nm)(0)(m))(function(ut){return function(dt){return Nt(lt)(2)([Ge(lt)(0)(_(g)(Pn()(Bn))(h))([$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:1e3,q:20})([ut])]),Ge(lt)(0)(_(g)(Pn()(Bn))(c))([$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:2e3,q:20})([ut])]),Ge(lt)(0)(_(g)(Pn()(Bn))(v))([_l(Dd(Et(ht()(J(J(Ct)($h)()()()({reflectSymbol:function(){return"q"}}))(ad)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:4e3,q:20})([ut])])])}})]})};return Zr([Zr([pe("tempo"),Bo(yp(gu)(E(D))(l(C(V)))(ru(gu)(tt(ci)(Lo.value)("range"))(function(){return ru(gu)(tt(Tc)(Wi.value)("0"))(function(){return ru(gu)(tt(Ec)(Ui.value)("100"))(function(){return ru(gu)(tt(xc)(Zi.value)("1"))(function(){return ru(bp)(tt(Fc)(zi.value)("50"))(function(){return tt(Sc)(qi.value)(ne(function(){var o=ce($)(te)(bf(Vn)(Yc)(n.slider)),p=zn(Oa)(Qf);return function(s){return o(p(Fl(s)))}}()))})})})})})))([])]),wn(gn(Bt)(E(D))(_(g)(function(){var o=tt(ye)(De.value);return function(p){return o(ne(T(p)))}}()))([Vr(g)(Nn(Jt(V))(R(N(D))(l(C(V))(l($)(void 0)))(_(g)(function(o){return o.value0})(e)))(K(g)(u)(rt(et))))(function(o){return function(){o();var s=ma(se)(),f=Hi(g)(nt.create)(O1)(M0(s)(.91)(_(g)(qa(0)(.42)(100)(1.4))(_u(a.slider)))),m=yl(s)(i(f))(),v=Q(ot)(m)(Mn(se)(s));return t(Q(ot)(v)(n.startStop.start(void 0)))(),n.startStop.stop(Q(ot)(v)(Mn(se)(s)))()}}),Vr(g)(a.startStop.stop)(function(o){return Q(ot)(o)(Q(ot)(t(l($)(void 0)))(n.startStop.start(void 0)))})]))([sn(Se(Bt)(E(D))([K(g)(u)("Turn on"),K(g)(a.startStop.stop)("Turn off")]))])])}}))})}}};var P1=function(){return d.value}(),kT=function(){return Dr({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(Y)(d.value)(P1)({})}();var R1=function(){return d.value}(),gT=function(){return Dr({reflectType:function(){return`<section>
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
</section>`}})({reflectType:function(){return"@"}})()()(Y)(d.value)(R1)({})}();var L1=function(){return d.value}(),CT=function(){return Dr({reflectType:function(){return`<section>

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
</section>`}})({reflectType:function(){return"@"}})()()(Y)(d.value)(L1)({})}();var H1=function(){return d.value}(),hT=function(t){return function(r){return function(e){return function(n){var a=function(i){return Ua(N(D))(C(V))(n)(Q(ot)(r(i))(dn))},u=Wa(t)(e);return Dr({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(H()(H()(H()(H()(H()(mn()(H()(Y)({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"inOcarina"}})({reflectSymbol:function(){return"inOcarina"}}))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}))(d.value)(H1)({next:a(op.value),primer:B(CT),inOcarina:B(gT),flavors:B(kT),ex0:B(mT(u)(r)(n)),ex1:B(yT(u)(r)(n)),ex2:B(AT(u)(r)(n))})}}}};var W1=function(){return d.value}(),ET=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(W1)({ai0:B(wt(e)(t)(function(n){return Io(Un)(Ut(Ni)(Ut(Ni)(Ut(Ni)(_(dl)(function(a){return function(u){return function(i){return function(o){return{tink0:a,tink1:u,tink2:i,tink3:o}}}}})(Hn(Un)(Tt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Hn(Un)(Tt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Hn(Un)(Tt(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Hn(Un)(Tt(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return yt(n)([Nt(lt)(1)(function(){var u=function(i){return l(C(G))(pf()(ef)(rm()(Br(Fa)(i))(z_)))};return[na(Ya)(a.tink0)(u(.1)),na(Ya)(a.tink1)(u(.2)),na(Ya)(a.tink2)(u(.9)),na(Ya)(a.tink3)(u(1.8))]}())])}}))})}}};var Z1=function(){return d.value}(),TT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(Z1)({ai0:B(wt(e)(t)(function(n){return Io(Un)(Ut(Ni)(Ut(Ni)(Ut(Ni)(_(dl)(function(a){return function(u){return function(i){return function(o){return{tink0:a,tink1:u,tink2:i,tink3:o}}}}})(Hn(Un)(Tt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Hn(Un)(Tt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Hn(Un)(Tt(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Hn(Un)(Tt(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return yt(n)([Nt(lt)(1)(function(){var u=function(o){return l(C(G))(pf()(ef)(rm()(Br(Fa)(o))(z_)))},i=function(o){var p=uu(so)(o)(4);return p===0?a.tink0:p===1?a.tink1:p===2?a.tink2:a.tink3};return Vr($r)(yn(0)(100))(function(o){var p=jr(o);return na(Ya)(i(o))(u(.3+.3*(p*vc(1.005)(p))))})}())])}}))})}}};var V1=function(){return d.value}(),ST=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(V1)({ai0:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Ba(sr(Er)(a)(vt()))(function(u){return function(i){return Nt(lt)(.8)([$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:400,q:1})([u]),$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:880,q:5})([u]),$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:1200,q:10})([u]),$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:2e3,q:20})([u]),$n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:3e3,q:30})([u])])}})])}}))})}}};var J1=function(){return d.value}(),xT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(J1)({ai0:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Ba(sr(Er)(a)(vt()))(function(u){return function(i){return Nt(lt)(.8)(Vr($r)(yn(0)(40))(Ra(pn)(jr)(function(o){return $n(Dn(Et(ht()(J(J(Ct)(On)()()()({reflectSymbol:function(){return"q"}}))(vn)()()()({reflectSymbol:function(){return"frequency"}})))(gt()())))({frequency:200+o*150,q:30})([u])})))}})])}}))})}}};var X1=function(){return d.value}(),FT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(X1)({ai0:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return yt(n)([vu(function(u){return Nt(lt)(1)([na(Ya)(a)(vt()),Po(nn)(.1)([Nt(lt)(.6)([u])])])})])}}))})}}};var K1=function(){return d.value}(),Y1=function(t){return function(r){return l(t)(Pn(r)(Bn)({p:[1,1,0],o:0,d:10}))}},tB=function(t){return function(r){return l(t)(Pn(r)(Bn)({p:[1,1,0],o:0,d:8}))}},Ol=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return Po(t)(n)([Nt(r)(a)([_l(e)(u)(i)])])}}}}}}},OT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(d.value)(K1)({txt:B(pe(`dgh d g h i =
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
  ]`)),ai0:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return yt(n)([Ba(na(Ya)(a)(vt()))(function(u){return function(i){return vu(function(o){return Nt(lt)(1)([u,Ol(nn)(lt)(mu)(.15)(.7)(1500)([vu(function(p){return Ge(lt)(1)(Y1(C(G))())([Ol(nn)(lt)(mu)(.4)(.5)(2500)([o,p])])})]),Ol(nn)(lt)(mu)(.29)(.85)(2e3)([vu(function(p){return Nt(lt)(1)([Ol(nn)(lt)(mu)(.6)(.6)(3500)([o,vu(function(s){return Ge(lt)(1)(tB(C(G))())([Ol(nn)(lt)(mu)(.75)(.6)(4e3)([p,s]),Ol(nn)(lt)(mu)(.75)(.55)(3e3)([u])])})])])})])])})}})])}}))})}}};var eB=function(){return d.value}(),$T=function(t){return function(r){return function(e){return function(n){var a=function(u){return Ua(N(D))(C(V))(n)(Q(ot)(r(u))(dn))};return Dr({reflectType:function(){return`<section>
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
  </section>`}})({reflectType:function(){return"@"}})()()(mn()(Y)({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}}))(d.value)(eB)({hwLink:a(ff.value)})}}}};var aB=function(){return d.value}(),wT=function(t){return function(r){return function(e){return function(n){var a=function(i){return Ua(N(D))(C(V))(n)(Q(ot)(r(i))(dn))},u=Wa(t)(e);return Dr({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(H()(H()(H()(H()(H()(H()(H()(mn()(Y)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}}))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}}))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}}))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}}))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}}))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}}))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}}))(d.value)(aB)({intro:B($T(t)(r)(e)(n)),next:a(ap.value),code0:B(ET(u)(r)(n)),code1:B(TT(u)(r)(n)),code2:B(ST(u)(r)(n)),code3:B(xT(u)(r)(n)),code4:B(FT(u)(r)(n)),code5:B(OT(u)(r)(n))})}}}};var MT=function(t){return function(r){return new M(X("code")(t)(U(r)))}},uy=MT(O(E(D)));var PT=function(t){return function(r){return new M(X("pre")(t)(U(r)))}},oy=PT(O(E(D)));var cB=function(){return d.value}(),IT=function(t){return function(r){return function(e){return function(n){var a=Q(ot)(r(up.value))(dn),u=Wa(t)(e);return Dr({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(H()(mn()(H()(Y)({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}}))(d.value)(cB)({code:B(oy([uy([pe(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])])),result:B(wt(n)(u)(function(i){return l(Ta)(void 0)})(function(i){return function(o){return yt(i)([Nt(lt)(.15)([cf(Ac)(440)(vt())])])}})),next:Ua(N(D))(C(V))(n)(a)})}}}};var RT=Hf;var NT=function(){return function(t){return t}},LT=function(){return function(t){return t}};var iy=function(){function t(){}return t.value=new t,t}();var BT={attr:function(t){return function(r){return b({key:"height",value:q(r)})}}};var cy=function(){function t(){}return t.value=new t,t}();var HT={attr:function(t){return function(r){return b({key:"width",value:q(r)})}}};var fy=function(t){return function(r){return new M(X("canvas")(t)(U(r)))}};var ly=function(){function t(){}return t.value=new t,t}();var _y={attr:function(t){return function(r){return b({key:"@self@",value:st(r)})}}};function Nm(t){return function(){return t.getContext("2d")}}function Ap(t){return function(r){return function(){t.fillStyle=r}}}function Lm(t){return function(){t.beginPath()}}function Bm(t){return function(){t.fill()}}function py(t){return function(r){return function(){t.arc(r.x,r.y,r.radius,r.start,r.end,r.useCounterClockwise)}}}function Hm(t){return function(r){return function(){t.fillRect(r.x,r.y,r.width,r.height)}}}var TB=function(){return 2*Dc}(),$l=function(t){return{o:t.value0+.04,n:t.value1,t:ei}};var SB=function(){return d.value}(),wl=function(t){return function(r){return function(e){return function(n){return l(t)(No(r)(Bn)({p:[e,n],o:0,d:16}))}}}},xB=function(t){return function(r){return l(t)(Pn(r)(Bn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},FB=function(t){return function(r){return l(t)(Pn(r)(Bn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}};var Um=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return function(o){return function(p){return function(s){return rp(t)(n)(a)([Ge(r)(u)(i)([Cd(e)(o)(p)(s)])])}}}}}}}}}},UT=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(i){return function(o){return function(p){return function(s){return rp(t)(n)(a)([Ge(r)(u)(i)([gd(e)(o)(p)(s)])])}}}}}}}}}},OB=function(t){return function(r){return function(e){return function(n){return l(t)(Cl(r)(Bn)({p:[e,n],o:0,d:16}))}}}},WT=400,sy=jr(WT),$B=function(){return jt(au)(WT)+"px"}(),qT=600,my=jr(qT),wB=function(){return jt(au)(qT)+"px"}(),MB={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},ZT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(d.value)(SB)({ex1:B(to()()(Wr({reflectSymbol:function(){return"canvas"}})(qr(_e))(Wr({reflectSymbol:function(){return"slider"}})(qr(_e))(Wr({reflectSymbol:function(){return"startStop"}})(qr(Yu(pu()(Wr({reflectSymbol:function(){return"loading"}})(qr(_e))(Wr({reflectSymbol:function(){return"start"}})(qr(_e))(Wr({reflectSymbol:function(){return"stop"}})(qr(_e))(Xn)()())()())()()))))(Xn)()())()())()())(fe({reflectSymbol:function(){return"canvas"}})()()(fe({reflectSymbol:function(){return"slider"}})()()(ju({reflectSymbol:function(){return"startStop"}})()()()(fe({reflectSymbol:function(){return"loading"}})()()(fe({reflectSymbol:function(){return"start"}})()()(fe({reflectSymbol:function(){return"stop"}})()()(jn)()())()())()())(jn)()())()())()())(d.value)(function(n){return function(a){var u=R(N(D))(l(C(V))(void 0))(a.startStop.start),i=function(o){return function(p){return function(s){var f=_(g)(function(m){return new nt(m.acTime,m.value)})(El(o)(_u(a.slider)));return[pm(_m(Et(ht()(J(J(Ct)(lm)()()()({reflectSymbol:function(){return"fftSize"}}))(fm)()()()({reflectSymbol:function(){return"cb"}})))(gt()())))({cb:function(m){return function(){return An(new F(m))(s)(),An(W.value)(s)}},fftSize:Ks.value})(l(de)(Ba(na(Ya)(p)(R(N($))(vt())(_(g)(function(){var m=va()(We),v=ra(Fn)(qa(0)(.96)(100)(1.04));return function(c){return m($l(v(c)))}}())(f))))(function(m){return function(v){return vu(function(c){return Nt(lt)(1)([m,rp(dd(Et(ht()(J(J(Ct)(wh)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(ud)()()()({reflectSymbol:function(){return"delayTime"}})))(gt()())))({maxDelayTime:2.5,delayTime:1})(_(g)(function(){var h=Cl()(We),ut=ra(Fn)(qa(0)(.5)(100)(2.45));return function(dt){return h($l(ut(dt)))}}())(f))([Ge(lt)(.4)(_(g)(function(){var h=Pn()(We),ut=ra(Fn)(qa(0)(.6)(100)(.9));return function(dt){return h($l(ut(dt)))}}())(f))([m])]),Um(nn)(lt)(mu)(.15)(O(E($)))(.7)(O(E($)))(1500)(wl(C(G))()(1500)(3e3))([vu(function(h){return Ge(lt)(1)(xB(C(G))())([Um(nn)(lt)(mu)(.4)(O(E($)))(.5)(O(E($)))(3e3)(wl(C(G))()(3e3)(100))([c,h])])})]),Um(nn)(lt)(mu)(.29)(_(g)(function(){var h=Cl()(We),ut=ra(Fn)(qa(0)(.1)(100)(.4));return function(dt){return h($l(ut(dt)))}}())(f))(.85)(O(E($)))(2e3)(wl(C(G))()(2e3)(5e3))([vu(function(h){return Nt(lt)(1)([Um(nn)(lt)(mu)(.6)(_(g)(function(){var ut=Cl()(We),dt=ra(Fn)(qa(0)(.8)(100)(.3));return function(fr){return ut($l(dt(fr)))}}())(f))(.6)(O(E($)))(3500)(wl(C(G))()(3500)(100))([c,vu(function(ut){return Ge(lt)(1)(FB(C(G))())([UT(nn)(lt)(bd)(.75)(_(g)(function(){var dt=Cl()(We),fr=ra(Fn)(qa(0)(.9)(100)(.1));return function(Zt){return dt($l(fr(Zt)))}}())(f))(.6)(O(E($)))(4e3)(wl(C(G))()(4e3)(200))([h,ut]),UT(nn)(lt)(bd)(.75)(OB(C(G))()(.75)(.2))(.55)(O(E($)))(200)(wl(C(G))()(200)(4e3))([m])])})])])})])])})}})))]}}};return Zr([fy(R(N(D))(gn(Bt)(E(D))(l(C(V)))([tt(HT)(cy.value)(wB),tt(BT)(iy.value)($B),tt(cg)(Xt.value)("width: 100%;"),tt(_y)(ly.value)(function(){var o=ce($)(te)(function(p){return function(){var f=Nm(p)();return Ap(f)("black")(),Hm(f)({width:my,height:sy,x:0,y:0})(),void 0}});return function(p){return o(TD(p))}}())]))(_(g)(function(o){return tt(_y)(ly.value)(function(){var p=ce($)(te)(function(s){return function(){var m=Nm(s)();return Ap(m)("black")(),Hm(m)({width:my,height:sy,x:0,y:0})(),Ap(m)("rgba(255,255,255,0.2)")(),ql(o)(function(v){return function(){return Lm(m)(),py(m)({end:TB,radius:v.value1*40,start:0,x:v.value0.x*my,y:v.value0.y*sy,useCounterClockwise:!1})(),Bm(m)()}})()}});return function(s){return p(TD(s))}}())})(a.canvas)))([]),Bo(gn(Bt)(E(D))(l(C(V)))([tt(ci)(Lo.value)("range"),tt(Tc)(Wi.value)("0"),tt(Ec)(Ui.value)("100"),tt(xc)(Zi.value)("1"),tt(Fc)(zi.value)("50"),tt(ig)(Xt.value)("width: 100%;"),tt(Sc)(qi.value)(ne(function(){var o=ce($)(te)(bf(Vn)(Yc)(n.slider)),p=zn(Oa)(Qf);return function(s){return o(p(Fl(s)))}}()))]))([]),wn(Se(Bt)(E(D))([l(C(V))(tt(Uf)(Xt.value)("width:100%; padding:1.0rem;")),gn(Bt)(E(D))(_(g)(function(){var o=tt(ye)(De.value);return function(p){return o(ne(T(p)))}}()))([K(g)(a.startStop.loading)(l($)(void 0)),Vr(g)(a.startStop.stop)(function(o){return Q(ot)(o)(Q(ot)(t(l($)(void 0)))(n.startStop.start(void 0)))}),Vr(g)(Nn(Jt(V))(R(N(D))(l(C(V))(l($)(void 0)))(_(g)(function(o){return o.value0})(e)))(K(g)(u)(rt(et))))(function(o){return function(){o(),n.startStop.loading(void 0)();var s=ie(W.value)(),f=Ro(P(He)(ma(Be))(function(m){return P(He)(oo(Be)(m))(function(v){return P(He)(_(Ri)(LT())(SE(Un)(RT)(Tt(m))(NT()(MB))))(function(c){return P(He)(Ae(Be)(E_(0)(5e4)))(function(h){var ut=Xf(hD(ya(x_(c.pluck0))(Gf(Xv(Qv()(c))))))({newSeed:Vf(h),size:4});return Ae(Be)(function(){var fr=Rn(Do)($)(function(pt){return function(){var Fr=eo(),ir=eo();return{x:Fr,y:ir}}})(yn(0)(127))(),Zt=yt(m)(i(m)(ut)(s))(),Kr=$t(gc)(function(pt){return function(){var Fr=he(s)();return Jn($)(te)(Fr)(function(ir){return function(){var Ho=lp(ir)(),io=_(S)(function(){var Vi=r_(fr),Cu=_($r)(function(bn){return function(li){return li/255}(bn)});return function(bn){return Vi(Cu(bn))}}())(Em(hm)(Ho))();return n.canvas(io)(),void 0}})()}})(),re=Q(ot)(Q(ot)(Q(ot)(Zt)(v))(Mn(se)(m)))(Kr);return n.startStop.stop(re)(),re})})})})}))();return t(function(){return n.startStop.start(void 0)(),ui(Li(f))()})(),void 0}})])]))([sn(Se(Bt)(E(D))([_(g)(T("Turn off"))(a.startStop.stop),_(g)(T("Turn on"))(u),_(g)(T("Loading..."))(a.startStop.loading)]))])])}}))})}}};var IB=function(){return d.value}(),zT=function(t){return function(r){return function(e){return function(n){var a=Wa(t)(e);return It({reflectType:function(){return`<div>
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
</div>`}})()()(H()(mn()(Y)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}}))(IB)({next:Ua(N(D))(C(V))(n)(Q(ot)(r(ff.value))(dn)),ex:B(ZT(a)(r)(n))})}}}};var NB=function(){return d.value}(),VT=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<div>
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
</div>`}})()()(mn()(Y)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(NB)({next:l(C(V))(tt(ye)(De.value)(ne(T(Q(ot)(r(Dm.value))(dn)))))})}}}};var BB=function(){return d.value}(),GT=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}}))(BB)({txt:B(pe(`\\ctx buf -> run2 ctx
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
  ]`)),cancel:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Nt(lt)(1)([sr(Er)(a)(Se(Bt)(E($))([vt(),Ju(1e3)(l(C(G))(va()(Bn)({p:eu(mi)(K($r)(yn(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),Ju(3e3)(l(C(G))(va()(bh)({o:3.5})))]))])])}}))})}}};var UB=function(){return d.value}(),JT=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}}))(UB)({txt:B(pe(`\\ctx buf -> run2 ctx
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
  ]`)),envelope:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Nt(lt)(1)([sr(Er)(a)(Se(Bt)(E($))([vt(),Ju(1e3)(l(C(G))(va()(Bn)({p:eu(mi)(K($r)(yn(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}}))})}}};var qB=function(){return d.value}(),jT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}}))(d.value)(qB)({numericEx:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Nt(lt)(1)([sr(Er)(a)(fi(N($))(vt())(function(){return fi(N($))(Ju(1e3)(fi(N($))(l(C(G))(va()(We)({n:1,o:1,t:td})))(function(){return l(C(G))(va()(We)({n:1.3,o:2,t:ei}))})))(function(){return Ju(2500)(fi(N($))(l(C(G))(va()(We)({n:1,o:2.5,t:td})))(function(){return l(C(G))(va()(We)({n:.7,o:3.5,t:dh}))}))})}))])])}}))})}}};var zB=function(){return d.value}(),XT=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(H()(Y)({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}}))(d.value)(zB)({suddenEx:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([Nt(lt)(1)([sr(Er)(a)(Se(Bt)(E($))([vt(),Ju(1500)(l(C(G))(va()(vh)({n:1.4})))]))])])}}))})}}};var GB=function(){return d.value}(),QT=function(t){return function(r){return function(e){return It({reflectType:function(){return`<section>
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
`}})()()(H()(Y)({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}}))(GB)({unitEx:B(wt(e)(t)(function(n){return Tt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return yt(n)([sr(Er)(a)(Se(Bt)(E($))([vt(),l(C(G))(va()(ph(Oi)(Oi))(_h(Nt(lt)(1)([mm(am)(1)(vt()),Nt(lt)(.2)([pl(om)(100)([Y_(el)(50)(vt())])])]))))]))])}}))})}}};var jB=function(){return d.value}(),KT=function(t){return function(r){return function(e){return function(n){var a=Q(ot)(r(ip.value))(dn),u=Wa(t)(e);return It({reflectType:function(){return`<div>
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
</div>`}})()()(H()(H()(mn()(H()(H()(H()(Y)({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}}))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}}))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}}))(jB)({sudden:B(XT(u)(r)(n)),numeric:B(jT(u)(r)(n)),envelope:B(JT(u)(r)(n)),cancel:B(GT(u)(r)(n)),unit:B(QT(u)(r)(n)),next:Ua(N(D))(C(V))(n)(a)})}}}};var QB=function(){return d.value}(),YT=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<div>
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
</div>`}})()()(mn()(Y)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(QB)({next:l(C(V))(tt(ye)(De.value)(ne(T(Q(ot)(r(dm.value))(dn)))))})}}}};var YB=function(){return d.value}(),tS=function(t){return function(r){return function(e){return function(n){return It({reflectType:function(){return`<div>
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
</div>`}})()()(mn()(Y)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(YB)({next:l(C(V))(tt(ye)(De.value)(ne(T(Q(ot)(r(lf.value))(dn)))))})}}}};var rH=function(){return d.value}(),rS=function(t){return function(r){return function(e){return function(n){return Dr({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out ocarina! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-ocarina">Ocarina GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Now go out there and play some ocarina!</p>
</div>`}})({reflectType:function(){return"~"}})()()(Y)(d.value)(rH)({})}}}};var nH=`module Main where

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
`,aH=ku(function(t){return Ht(function(r){return $t(t)(function(e){return function(){var a=eo();return r(e(a))()}})})}),uH=function(){return d.value}(),oH="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",eS=function(t){return function(r){return function(e){return Dr({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels lock payload))</code>. Streaming audio is a data type with two constructors: <code>sound</code> to create a subgraph and <code>silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: ocarina automatically frees up resources when you send the <code>silence</code> event. Note that, once you turn a subraph off with <code>silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(H()(H()(Y)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(d.value)(uH)({txt:B(pe(nH)),ex1:B(to()()(Wr({reflectSymbol:function(){return"slider"}})(qr(_e))(Wr({reflectSymbol:function(){return"startStop"}})(qr(Yu(pu()(Wr({reflectSymbol:function(){return"loading"}})(qr(_e))(Wr({reflectSymbol:function(){return"start"}})(qr(_e))(Wr({reflectSymbol:function(){return"stop"}})(qr(_e))(Xn)()())()())()()))))(Xn)()())()())(fe({reflectSymbol:function(){return"slider"}})()()(ju({reflectSymbol:function(){return"startStop"}})()()()(fe({reflectSymbol:function(){return"loading"}})()()(fe({reflectSymbol:function(){return"start"}})()()(fe({reflectSymbol:function(){return"stop"}})()()(jn)()())()())()())(jn)()())()())(d.value)(function(n){return function(a){var u=R(N(D))(l(C(V))(void 0))(a.startStop.start),i=Hi(g)(nt.create)(aH)(Gu(Jt(G))(function(p){return function(s){return s+1|0}})(_u(a.slider))(0)),o=function(p){return[Nt(lt)(1)([is(_(g)(function(s){return Se(Bt)(E($))([l(C(G))(fh(na(X_(Et(ht()(J(J(Ct)(Eh)()()()({reflectSymbol:function(){return"playbackRate"}}))(G_)()()()({reflectSymbol:function(){return"buffer"}})))(gt()())))({buffer:p,playbackRate:.7+Ja(s)*2})(vt()))),Ju(5e3)(l(C(G))(lh))])})(i))])]};return Zr([Zr([pe("Slide me!"),Bo(gn(Bt)(E(D))(l(C(V)))([tt(ci)(Lo.value)("range"),tt(Tc)(Wi.value)("0"),tt(Ec)(Ui.value)("100"),tt(xc)(Zi.value)("1"),tt(Fc)(zi.value)("50"),tt(Sc)(qi.value)(ne(T(n.slider(void 0))))]))([])]),wn(gn(Bt)(E(D))(_(g)(function(){var p=tt(ye)(De.value);return function(s){return p(ne(T(s)))}}()))([K(g)(a.startStop.loading)(l($)(void 0)),Vr(g)(a.startStop.stop)(function(p){return Q(ot)(p)(Q(ot)(t(l($)(void 0)))(n.startStop.start(void 0)))}),Vr(g)(Nn(Jt(V))(R(N(D))(l(C(V))(l($)(void 0)))(_(g)(function(p){return p.value0})(e)))(K(g)(u)(rt(et))))(function(p){return function(){p(),n.startStop.loading(void 0)();var f=Ro(P(He)(ma(Be))(function(m){return P(He)(oo(Be)(m))(function(v){return P(He)(Tt(m)(oH))(function(c){return Ae(Be)(function(){var ut=Cm(o(c))(),dt=Q(ot)(Q(ot)(ut)(v))(Mn(se)(m));return n.startStop.stop(dt)(),dt})})})}))();return t(function(){return n.startStop.start(void 0)(),ui(Li(f))()})(),void 0}})]))([sn(Se(Bt)(E(D))([_(g)(T("Turn off"))(a.startStop.stop),_(g)(T("Turn on"))(u)]))])])}}))})}}};var cH=function(){return d.value}(),nS=function(t){return function(r){return function(e){return function(n){var a=Wa(t)(e);return It({reflectType:function(){return`<div>
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
</div>`}})()()(H()(H()(Y)({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}}))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}}))(cH)({appl:B(gm("\u{1F44F}")(n)(a)(function(u){return Tt(u)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(u){return function(i){return yt(u)([Nt(lt)(1)([sr(Er)(i)(vt())])])}})),suby:B(eS(a)(r)(n))})}}}};var $Bt=function(t){return t},wBt={Coercible0:function(){}},lH=function(){var t=function(r){var e=function(n){if(n instanceof vm)return Zr(l(de)(rn(zT(r.setCancellation)(r.setPage))));if(n instanceof ff)return Zr(l(de)(rn(IT(r.setCancellation)(r.setPage))));if(n instanceof up)return Zr(l(de)(rn(wT(r.setCancellation)(r.setPage))));if(n instanceof ap)return Zr(l(de)(rn(O0(r.setCancellation)(r.setPage))));if(n instanceof Dm)return Zr(l(de)(rn(tS(r.setCancellation)(r.setPage))));if(n instanceof lf)return Zr(l(de)(rn(hT(r.setCancellation)(r.setPage))));if(n instanceof op)return Zr(l(de)(rn(KT(r.setCancellation)(r.setPage))));if(n instanceof ip)return Zr(l(de)(rn(pT(r.setCancellation)(r.setPage))));if(n instanceof dm)return Zr(l(de)(rn(rS(r.setCancellation)(r.setPage))));if(n instanceof HE)return Zr(l(de)(rn(VT(r.setCancellation)(r.setPage))));if(n instanceof cp)return Zr(l(de)(rn(nS(r.setCancellation)(r.setPage))));if(n instanceof UE)return Zr(l(de)(rn(YT(r.setCancellation)(r.setPage))));throw new Error("Failed pattern match at Ocarina.Example.Docs (line 142, column 5 - line 142, column 76): "+[n.constructor.name])};return e(r.page)};return rn(function(r){return function(e){var n=Gu(Jt(V))(function(a){if(a instanceof fp)return function(u){return{prevPage:new F(u.curPage),curPage:a.value0,cancel:u.cancel,pageChange:!0}};if(a instanceof Sd)return function(u){return{cancel:a.value0,pageChange:!1,curPage:u.curPage,prevPage:u.prevPage}};throw new Error("Failed pattern match at Ocarina.Example.Docs (line 54, column 15 - line 56, column 83): "+[a.constructor.name])})(e)({prevPage:W.value,curPage:vm.value,cancel:l($)(void 0),pageChange:!0});return Zr([Zr(_($r)(function(a){return iD([oD(R(N(D))(gn(Bt)(E(D))(l(C(V)))([tt(ye)(De.value)(ne(T(r(new fp(a.value0))))),tt(fg)(Xt.value)("cursor:pointer;")]))(_(g)(function(u){return tt(ye)(De.value)(ne(T(function(){return u.cancel(),r(new fp(a.value0))()})))})(i_(Fu(D))(function(){var u=fu(Ga);return function(i){return u(function(o){return o.pageChange}(i))}}())(n))))([pe(a.value1.value0)]),qf(l(C(V))(tt(ms)(Xt.value)(function(){return a.value1.value1?"":"display:none;"}())))([pe(" | ")])])})([new nt(vm.value,new nt("Home",!0)),new nt(ff.value,new nt("Hello world",!0)),new nt(up.value,new nt("Array, fan, and fix",!0)),new nt(ap.value,new nt("Audio units",!0)),new nt(lf.value,new nt("Events",!0)),new nt(op.value,new nt("Parameters",!0)),new nt(ip.value,new nt("State",!0)),new nt(cp.value,new nt("Subgraphs",!1))])),ag(hr)(function(a){return t({page:a.curPage,setPage:function(u){return r(fp.create(u))},setCancellation:function(u){return r(Sd.create(u))}})})(i_(Fu(D))(function(a){return a.pageChange})(n))])}})}(),MBt=function(t){return{page:t,setPage:Pt(tn(Le(Ne))),setCancellation:Pt(tn(Le(Ne)))}},PBt=yC(lH);export{$Bt as TopLevelSg,PBt as main,wBt as newtypeTopLevelSg_,MBt as p2tl,lH as scene};
