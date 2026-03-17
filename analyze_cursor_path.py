import pandas as pd
import numpy as np
from pathlib import Path


def point_to_line_distance(point, line_start, line_end):
    """
    Calculate the perpendicular distance from a point to a line segment.
    
    Parameters:
    -----------
    point : array-like
        [x, y] coordinates of the point
    line_start : array-like
        [x, y] coordinates of line start
    line_end : array-like
        [x, y] coordinates of line end
    
    Returns:
    --------
    float
        Perpendicular distance from point to line
    """
    point = np.array(point)
    line_start = np.array(line_start)
    line_end = np.array(line_end)
    
    # Vector from line_start to line_end
    line_vec = line_end - line_start
    
    # Vector from line_start to point
    point_vec = point - line_start
    
    # Length of the line segment
    line_length = np.linalg.norm(line_vec)
    
    # Handle case where start and end are the same point
    if line_length == 0:
        return np.linalg.norm(point_vec)
    
    # Normalize the line vector
    line_unit = line_vec / line_length
    
    # Project point_vec onto the line
    projection_length = np.dot(point_vec, line_unit)
    
    # Calculate perpendicular distance
    # If projection is beyond the line segment, use distance to nearest endpoint
    if projection_length < 0:
        distance = np.linalg.norm(point - line_start)
    elif projection_length > line_length:
        distance = np.linalg.norm(point - line_end)
    else:
        # Calculate perpendicular distance
        projection = line_start + projection_length * line_unit
        distance = np.linalg.norm(point - projection)
    
    return distance


def analyze_trial(csv_file, target_start_cols=None, target_end_cols=None, 
                 phase_col='phase', movement_phase=None, target_size_col=None, passthrough_cols=None):
    """
    Analyze a single trial and calculate maximum deviation from straight-line path.
    
    Parameters:
    -----------
    csv_file : str or Path
        Path to the CSV file containing trial data
    target_start_cols : tuple, optional
        Column names for start target (e.g., ('target_start_x', 'target_start_y'))
        If None, uses first cursor position
    target_end_cols : tuple, optional
        Column names for end target (e.g., ('target_end_x', 'target_end_y'))
        If None, uses last cursor position
    phase_col : str, optional
        Name of the column indicating trial phase (default: 'phase')
    movement_phase : int, str, or None, optional
        Value in phase column indicating movement phase to analyze
        If None, analyzes entire trial
    target_size_col : str, optional
        Name of the column containing target radius
        If provided, subtracts target radii from straight line distance
    
    Returns:
    --------
    dict
        Dictionary containing analysis results:
        - max_deviation: maximum perpendicular distance from straight line
        - mean_deviation: average deviation across all points
        - path_length: actual path length traveled
        - straight_line_distance: direct distance between target centers
        - adjusted_straight_line_distance: distance minus target radii (if target_size_col provided)
        - efficiency: ratio of adjusted straight line distance to actual path length
    """
    # Read the CSV file
    df = pd.read_csv(csv_file)
    
    # Determine target positions (from full dataset)
    if target_start_cols is not None:
        start_target = np.array([df[target_start_cols[0]].iloc[0], 
                                df[target_start_cols[1]].iloc[0]])
    else:
        start_target = np.array([df['x'].iloc[0], df['y'].iloc[0]])
    
    if target_end_cols is not None:
        end_target = np.array([df[target_end_cols[0]].iloc[0], 
                              df[target_end_cols[1]].iloc[0]])
    else:
        end_target = np.array([df['x'].iloc[-1], df['y'].iloc[-1]])
    
    # Filter data by phase if specified
    if movement_phase is not None:
        if phase_col not in df.columns:
            raise ValueError(f"Phase column '{phase_col}' not found in CSV file")
        df_filtered = df[df[phase_col] == movement_phase].copy()
        if len(df_filtered) == 0:
            raise ValueError(f"No data found for phase '{movement_phase}'")
    else:
        df_filtered = df.copy()
    
    # Extract cursor positions from filtered data
    cursor_positions = df_filtered[['cursorx_cm', 'cursory_cm']].values
    
    # Calculate deviations for all cursor positions
    deviations = []
    for pos in cursor_positions:
        dist = point_to_line_distance(pos, start_target, end_target)
        deviations.append(dist)
    
    deviations = np.array(deviations)
    
    # Calculate path metrics
    max_deviation = np.max(deviations)
    mean_deviation = np.mean(deviations)
    
    # Calculate actual path length
    path_segments = np.diff(cursor_positions, axis=0)
    path_length = np.sum(np.linalg.norm(path_segments, axis=1))
    
    # Calculate straight line distance
    straight_line_distance = np.linalg.norm(end_target - start_target)
    
    # Adjust straight line distance by subtracting target radii if specified
    if target_size_col is not None:
        if target_size_col not in df.columns:
            raise ValueError(f"Target size column '{target_size_col}' not found in CSV file")
        end_target_radius = df[target_size_col].iloc[0]
        start_target_radius = 0.5  # Start target always has radius of 0.5 cm
        # Subtract start radius (0.5) and end radius from straight line distance
        adjusted_straight_line_distance = straight_line_distance - (start_target_radius + end_target_radius)
        # Make sure it doesn't go negative
        adjusted_straight_line_distance = max(0, adjusted_straight_line_distance)
    else:
        adjusted_straight_line_distance = straight_line_distance
    
    # Calculate efficiency using adjusted distance
    efficiency = adjusted_straight_line_distance / path_length if path_length > 0 else 0
    
    result = {
        'trial_file': Path(csv_file).name,
        'max_deviation': max_deviation,
        'mean_deviation': mean_deviation,
        'path_length': path_length,
        'straight_line_distance': straight_line_distance,
        'adjusted_straight_line_distance': adjusted_straight_line_distance,
        'efficiency': efficiency,
        'num_samples': len(cursor_positions)
    }
    
    # Add target radii to result if available
    if target_size_col is not None:
        result['start_target_radius'] = 0.5
        result['end_target_radius'] = end_target_radius
        
    # Add passthrough columns to result if specified
    if passthrough_cols is not None:
        for col in passthrough_cols:
            if col not in df.columns:
                print(f"Warning: Passthrough column '{col}' not found in {csv_file}")
                result[col] = None
            else:
                result[col] = df[col].iloc[0]
    
    return result


def analyze_all_trials(trial_directory, target_start_cols=None, target_end_cols=None, 
                       phase_col='phase', movement_phase=None, target_size_col=None, passthrough_cols=None,
                       pattern='*.csv'):
    """
    Analyze all trial CSV files in a directory.
    
    Parameters:
    -----------
    trial_directory : str or Path
        Directory containing trial CSV files
    target_start_cols : tuple, optional
        Column names for start target (e.g., ('target_start_x', 'target_start_y'))
    target_end_cols : tuple, optional
        Column names for end target (e.g., ('target_end_x', 'target_end_y'))
    phase_col : str, optional
        Name of the column indicating trial phase (default: 'phase')
    movement_phase : int, str, or None, optional
        Value in phase column indicating movement phase to analyze
        If None, analyzes entire trial
    target_size_col : str, optional
        Name of the column containing target radius
        If provided, subtracts target radii from straight line distance
    passthrough_cols : list of str, optional
        List of column names to include in output without calculation
        Values are taken from the first row of the CSV
    pattern : str, optional
        File pattern to match (default: '*.csv')
    
    Returns:
    --------
    pd.DataFrame
        DataFrame with analysis results for all trials
    """
    trial_dir = Path('/Users/aatkin/Documents/Tactile_Suppression_Study/hand_dual_data/274729_hand_dual')
    csv_files = sorted(trial_dir.glob(pattern))
    
    if not csv_files:
        print(f"No CSV files found in {trial_directory}")
        return None
    
    results = []
    for csv_file in csv_files:
        try:
            result = analyze_trial(csv_file, target_start_cols, target_end_cols,
                                 phase_col, movement_phase, target_size_col, passthrough_cols)
            results.append(result)
            print(f"Processed: {csv_file.name} - Max deviation: {result['max_deviation']:.2f}")
        except Exception as e:
            print(f"Error processing {csv_file.name}: {e}")
    
    return pd.DataFrame(results)


# Example usage
if __name__ == "__main__":
    # Example 1: Analyze only the movement phase with target size adjustment
    # Assuming your CSV has columns like: x, y, time, phase, Target Size, target_start_x, target_start_y, target_end_x, target_end_y
    
    trial_directory = "'/Users/aatkin/Documents/Tactile_Suppression_Study/hand_dual_data/274729_hand_dual'"  # Change this to your trial directory
    
    # Specify the phase value that indicates movement between targets
    # This could be an integer (e.g., 2) or string (e.g., 'movement') depending on your data
    movement_phase_value = 5  # Change this to match your phase coding
    
    # If your CSV has target columns, specify them:
    results_df = analyze_all_trials(
        trial_directory,
        target_start_cols=('startx_cm', 'starty_cm'),
        target_end_cols=('targetx_cm', 'targety_cm'),
        phase_col='phase',  # Name of your phase column
        movement_phase=5,  # Value indicating movement phase
        target_size_col='targetSize',  # Name of column containing target radius
        passthrough_cols=['trial_idx', 'movement_time', 'ballistic_y_error', 'strength', 'vibrotactileStimTime', 'response']  # Your columns here
    )
    
    # Example 2: Analyze entire trial without target size adjustment
    # results_df = analyze_all_trials(
    #     trial_directory,
    #     target_start_cols=('target_start_x', 'target_start_y'),
    #     target_end_cols=('target_end_x', 'target_end_y'),
    #     movement_phase=None  # This will analyze all data
    # )
    
    # Example 3: If targets are just the first and last cursor positions
    # results_df = analyze_all_trials(
    #     trial_directory,
    #     phase_col='phase',
    #     movement_phase=2,
    #     target_size_col='Target Size'
    # )
    
    if results_df is not None:
        # Display summary statistics
        print("\n" + "="*60)
        print("SUMMARY STATISTICS")
        print("="*60)
        print(f"\nNumber of trials analyzed: {len(results_df)}")
        print(f"\nMaximum deviation:")
        print(f"  Mean: {results_df['max_deviation'].mean():.2f}")
        print(f"  Std:  {results_df['max_deviation'].std():.2f}")
        print(f"  Min:  {results_df['max_deviation'].min():.2f}")
        print(f"  Max:  {results_df['max_deviation'].max():.2f}")
        
        print(f"\nPath efficiency (using adjusted straight line distance):")
        print(f"  Mean: {results_df['efficiency'].mean():.3f}")
        print(f"  Std:  {results_df['efficiency'].std():.3f}")
        
        print(f"\nStraight line distance (center to center):")
        print(f"  Mean: {results_df['straight_line_distance'].mean():.2f}")
        
        print(f"\nAdjusted straight line distance (minus target radii):")
        print(f"  Mean: {results_df['adjusted_straight_line_distance'].mean():.2f}")
        
        print(f"\nPath length:")
        print(f"  Mean: {results_df['path_length'].mean():.2f}")
        
        # Save results to CSV
        results_df.to_csv('/Users/aatkin/Desktop/trial_analysis_results.csv', index=False)
        print(results_df)
