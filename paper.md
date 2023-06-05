---
title: 'Gala: A Python package for galactic dynamics'
tags:
  - R
  - optical tweezers
  - myosin
  - muscle
  - molecular motors
authors:
  - name: Brent Scott
    orcid: 0000-0002-2567-6870
    corresponding: true # (This is how to denote the corresponding author)
    affiliation: "1, 2"
  - name: Chris Marang
    affiliation: 2
  - name: Edward P. Debold
    affiliation: 2
affiliations:
 - name: Department of Biochemistry & Molecular Biophyics, Washington University in St. Louis, USA
   index: 1
 - name: Department of Kinesiology, University of Massachusetts Amherst
   index: 2
date: 05 June 2023
bibliography: paper.bib
---
# Summary 

The laser trap is an *in vitro* experimental procedure that allows researchers to measure nanoscale movements of single proteins molecules that produce piconewtown forces. **lasertrapr** is an application whose goal is to automate the workflow related to processing and analysing laser trap data. The app is built as an R package, but the target audience is researchers of non-programming or non-technical computing backgrounds so interaction with the application is done through the front end GUI created with the {shiny} R package via the {golem} framework. 


# Background 

The laser trap (or optical tweezers) has been revolutionary to the field of single molecule biophysics. Originally developed by Arthur Ashkin of Bell Laboratories (Ashkin 1986) the laser trap was eventually adopted by biologists to study the interactions of single molecular motors (e.g. myosin, kinesin, dynein) with their molecular tracks (e.g. actin, microtubules) by use of a three-bead assay [@finer1994; @kojima1997]. These experiments permit researchers the ability to observe the interaction of two proteins within a millisecond-time and nanometer-spatial resolution providing unprecedented insight into the molecular machinery underlying a wide variety of biological functions including muscle contraction, intracellular cargo transport, and cell-division. Such experiments need to be performed with a low trap stiffness (0.02-0.04 pN/nm) as to not hinder the function or harm the integrity of the experimental proteins or setup. Since the position of a trapped bead is largely dominated by Brownian forces, a bead stuck in a trap with low stiffness has a large variance in its displacement signal as trap stiffness ($\alpha$~trap~) is inversely proportional to the variance ($\sigma$^2^) of the displacement signal (via the Equipartition Theorem) [@dupuis1997, @svoboda1994].

$$ \alpha_{trap} = \frac{ k_B T_k } { \sigma^2 }   $$

where k~B~ is the Boltzmann constant and T~k~ the temperature in Kelvin. The high variance of the baseline displacement signal combined with the dampening effects of viscous drag forces masks the underlying mechanics of the two proteins interacting and cycling through a mechanochemical scheme that is common amongst biological motors used in these assays which makes the analytical task of identifying these events of interests quite challenging.  

The variance of the displacement signal is a crucially important feature of single molecule laser trap data as the variance can be exploited to determine when protein interactions do occur. Since the biological motors used in these experiments are stiffer than the trapping laser, the interaction of the proteins can be characterized by a decrease in signal variance of the time series (position over time) signal, via Eq 1, which is also often accompanied by a displacement from the mean baseline position as in the case of a biological motor, like myosin, attaching to an actin filament and performing a powerstroke. In some cases, the signal-to-noise ratio of the baseline and event populations variance can exceed 2:1 which makes these interaction events readily discernible “by-eye”. However, while simple and easy, the analysis of data “by-eye” has been criticized in the past as this method was suggested to introduce subjectivity via user bias as evidenced by early inaccurate estimations of myosin’s displacement size [@finer1994; @molloy1995n]. This exemplifies the fact that while the laser trap is a powerful and advanced scientific instrument, the reliability and accuracy of the information that can be extracted from the resulting data is limited by the validity of the techniques and programs used to analyze the data.

# Statement of Need

While there are numerous techniques that can be used to identify binding events, a common theme between them is that most require advanced computer programming knowledge to implement. This is then compounded with a need to then automate those scripts by a preferential creation of user-friendly graphical user interfaces (GUIs). For most, the advanced computer skills required to build sophisticated analysis programs with GUIs are taught in classes that are not degree requirements for graduate students or researchers seeking degrees in many biology-related fields. This presents a technological barrier that hinders progress in understanding and interpretation of data for new students and researchers, and an additional monetary cost barrier is added to a laboratory if the creation of custom programs must be outsourced. And, even in these situations then a research group is left with a custom and un-supported "black-box" program.

Unfortunately, there are currently no completely open-source projects whose primary aim is to automate the workflow of analyzing laser trap data (calibrations, processing, event identification, ensemble average, and summarizing statistics) written with an open-sourced programming language. Although, it should be noted there have been recent publications of programs aimed at single molecule event identification, most notably the MATLAB based SPASM (Software for Precise Analysis of Single Molecules, @blackwell2021). However, while SPASM itself is an open-source program, the underlying MATLAB language is proprietary/closed source language with a steep financial barrier. 

Here, we present **lasertrapr**, and open-source program for automating the analysis of laser trap data written in R, a free and open-source programming language (hence lasertrapr = laser trap + R). The tool has an easy-to-use GUI provided by the R-Shiny web-framework package. One of the main benefits of having a tool built with R/Shiny [@rcoreteam2022; @chang2021] is that there is high portability of the app across different operating systems as it can be installed on Windows, MacOS, and Linux systems. Additionally, we do not view our application as a replacement or competitor to a program such as SPASM, but as an additional tool made available to the community that share *some* similar functionalities. Furthermore, **lasertrapr** fully embodies the notion of a free and open-source project whose main goal is automation and reproducibility of the entire workflow of analyzing laser trap data which includes folder and file creation and organization via a folder manager, signal calibrations, data cleaning and preparation, event identification of both single molecule and mini-ensemble data, ensemble averaging, generation of complete project summary statistics, and creation of publication quality figures. Lastly, the coexistence of multiple programs will only benefit the biophysics community by enabling researchers the ability to contribute to and use an analysis program best suited for their interests and experimental setups.
