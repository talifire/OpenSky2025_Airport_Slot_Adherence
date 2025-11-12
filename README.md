# OpenSky2025 Airport Slot Adherence
This repository contains the dataset and R scripts used to explore the relationship between Collaborative Decision Making (CDM) implementation and airport slot adherence, including Slot Tolerance Quantification Method.

The columns are as follows:
*`ADEP`: the departure airport ICAO code, for example `EDDF`
*`ADES`: the destination airport ICAO code, for example `EGLL`
*`PHASE`: the flight phase category within the slot allocation process (e.g., `ARR, DEP`)
*`BLOCK_TIME`: the scheduled block time of the flight, indicating the actual off-block time in UTC
*`SLOT_TIME`: the allocated airport slot time assigned to the flight in UTC
*`FIRST_ACCEPTED`: the timestamp of the first slot acceptance by the operator in UTC
*`FIRST_SUBMITTED`: the timestamp of the first slot submission by the operator in UTC

Notes on Data Confidentiality

To protect sensitive operational data, all airports except for the six studied airports — EDDF, EGLL, EIDW, EKCH, LGAV, and LTFM — have been replaced with randomly generated identifiers. This ensures confidentiality while preserving the analytical integrity of the dataset.
