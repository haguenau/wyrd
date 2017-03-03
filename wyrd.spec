############ wyrd.spec ############
# This is for debug-flavor. Do not remove. Package is stripped conditionally.
#%#define __os_install_post       %{nil}
# %#define __spec_install_post /usr/lib/rpm/brp-compress

%define name wyrd
%define version 1.4.1
%define sfx tar.gz
%define release 1
%define descr term based calendar frontend for remind

Summary: %{descr}
Name: %{name}
Version: %{version}
Release: %{release}
Source0: %{name}-%{version}.%{sfx}
Copyright: GPL
Group: System Environment/Libs
BuildRoot: %{_tmppath}/%{name}-%{version}-buildroot
#Prefix: %#{_prefix}
#URL: 

%description

%{descr}


%prep
case "${RPM_COMMAND:-all}" in
all)
%setup -q
;;
esac

%build

case "${RPM_COMMAND:-all}" in
all|config)
#export CFLAGS="$RPM_OPT_FLAGS -O1 -g" 
#./configure --prefix=%{prefix}
%configure
;;
esac

case "${RPM_COMMAND:-all}" in
all|config|build)
make
;;
esac

%install

case "${RPM_COMMAND:-all}" in
all|config|build|install)
mkdir -p $RPM_BUILD_ROOT/etc
make DESTDIR=$RPM_BUILD_ROOT install 
;;esac

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc COPYING README TODO
%{_prefix}/*
/etc/*


%changelog
* Fri Dec 26 2003 Richard Zidlicky <rz@linux-m68k.org> 
- created skel.spec




# arch-tag: DO_NOT_CHANGE_0dfb61c0-79d1-4f5a-a711-b24213b5dc62 
