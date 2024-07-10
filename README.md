# NUOPC_DART-cap-development
This repository is a part of SIParCS project, where we are trying to improve the interface between a comprehensive
Earth System model (NCAR’s Community Earth System Model, CESM) and a state-of-the-art
data assimilation tool. NCAR’s Data Assimilation Research Testbed (DART) is an ensemble DA
tool that has been heavily tested and used for weather forecasting, ocean prediction, climate
projections, flood prediction, parameter estimation, and other applications. In previous
applications, DART has relied on modifying so-called “restart” files – which must be written to
disk – to influence the evolution of numerical models. The attending I/O and model stop and
restart places substantial limitations on overall model-DA performance. The goal of this project
is to build and test novel capability for DART to access the model state in memory using the
National Unified Operational Prediction Capability (NUOPC) system API, which is used to exchange
information across Earth System components and well known for coupling model components in the Earth System models. This Work has been configured to run test on NCAR’s new HPE
Cray EX cluster, Derecho, which is a 19.87-petaflops system.

**What does it means to build NUOPC layer over DART?**
The NUOPC Layer is a software layer built on top of the Earth System Modeling framework (ESMF). ESMF is a high-performance modeling framework that provides data structures, interface, and operations suited for building coupled models from a set of components. NUOPC refines the capabilities of ESMF by providing a more precise definition of what it means for a model to be a component and how components should interact and share data in a coupled system. The NUOPC Layer software is designed to work with typical high-performance models in the Earth science domain, most of which are written in Fortran and are based on a distributed memory model of parallelism (MPI).

**How does it works?**
The NUOPC Layer implements a set of generic components that serve as building blocks that can be assembled together in different ways to build a coupled modeling application. In some cases, a generic component can be used as is, and in other cases the generic component must be specialized (customized) for a perticular model or application. Additionaly, the nUOPC Layer defines a set of rules for how components should behave and interact with each other. These technical rules form the backbone of interoperability. NUOPC defines this effective interoperability as the ability of a model component to execute without code changes in a driver that provides the fields that it requires, and to return with informative messages if its input requirements are not met. Any component that follows the NUOPC Layer technical rules are considered as a NUOPC Layer compliant.

Here, we only have NUOPC cap code for DART which is named as `dart_nuopc.F90`. This acts as a model component in ESMF Framework and we are testing this cap code using ESMX application which provides the executable driver. This can help simplify the first cap since we do not need to know all the details of ESMF infrastructure, making it easier to build and test code at the beginning.
